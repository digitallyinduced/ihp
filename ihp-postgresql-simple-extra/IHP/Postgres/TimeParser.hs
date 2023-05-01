{-# LANGUAGE BangPatterns, CPP, GADTs, OverloadedStrings, RankNTypes, RecordWildCards #-}
module IHP.Postgres.TimeParser where

import BasicPrelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Data.Bits ((.&.))
import Data.Char (ord)

import Data.Fixed (Pico, Fixed(MkFixed))
import Data.Time.Clock.Compat (NominalDiffTime)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Time.LocalTime.Compat as Local

--Simple newtype wrapper around a postgres interval bytestring.
newtype PGInterval = PGInterval ByteString deriving (Eq, Show)

-- The mapping of the "interval" bytestring into application
-- logic depends on the Interval Output Style of the postgres database
-- and the semantics of the interval quantity in the application code (if they differ from the postgres
-- interpretation for some reason).
-- This module provides the PGInterval wrapper type and a parser for the default
-- `postgres` output style into a a PGTimeInterval data struct of years, months, days, and NominalDiffTime.
--  These can be combined with the standard Calendar/Time library to perform Calendar Arithmetic
--  Days and Years are big Integers and can be added to the Gregorian year and Julian Day respectively
--  Months are small Int (up to 1-11) denoting a month of the year, and the pgClock is a Nominal DiffTime
--  representing the time as measured by a clock without leap seconds.

data PGTimeInterval = PGTimeInterval { pgYears :: !Integer
                                     , pgMonths :: !Int
                                     , pgDays :: !Integer
                                     , pgClock :: !NominalDiffTime } deriving (Eq, Show)

-- To support the default postgres output style PGInterval -> PGTimeInterval
-- in Application Code we provide the parser combinators for the `postgres` output style.
-- for parsing (optional combination of): Y year[s] M mon[s] D day[s] [-]HH:MM:SS.[SSSs].
-- This corresponds to the default interval `postgres` format.
-- (https://www.postgresql.org/docs/current/datatype-datetime.html).
-- alternative parsers would need to be provided for the `sql_standard`, `postgres_verbose`, and `iso_8601`
-- styles/

unpackInterval :: PGInterval -> PGTimeInterval
unpackInterval (PGInterval bs) = case parseOnly pPGInterval bs of
    Left err -> error ("Couldn't parse PGInterval. " <> err)
    Right val -> val


pPGInterval :: Parser PGTimeInterval
pPGInterval = do
    year <- option 0 ((signed decimal <* space <* (string "years" <|>  string "year")))
    skipSpace
    mons <-  option 0 ((signed decimal <* space <* (string "mons" <|>  string "mon")))
    skipSpace
    days <- option 0 ((signed decimal <* space <* (string "days" <|>  "day")))
    skipSpace
    timeOfDay <- option 0 nominalDiffTime
    pure (PGTimeInterval year mons days timeOfDay)


-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  pure $! c2d a * 10 + c2d b


-- Take from Postgresql Internal to facilitate a definition of a NominalDiffTime FromField
-- | See https://stackoverflow.com/questions/32398878/converting-postgres-interval-to-haskell-nominaltimediff-with-postgresql-simple
-- |
-- Module:      Database.PostgreSQL.Simple.Time.Internal.Parser
-- Copyright:   (c) 2012-2015 Leon P Smith
--              (c) 2015 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Parsers for parsing dates and times.

toPico :: Integer -> Pico
toPico = MkFixed
-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.

pClockInterval :: Parser Local.TimeOfDay
pClockInterval = do
  h <- twoDigits <* char ':'
  m <- twoDigits
  mc <- peekChar
  s <- case mc of
         Just ':' -> anyChar *> seconds
         _   -> return 0
  if h < 24 && m < 60 && s <= 60
    then return (Local.TimeOfDay h m s)
    else fail "invalid time"

-- | Parse a count of seconds, with the integer part being two digits
-- long.
seconds :: Parser Pico
seconds = do
  real <- twoDigits
  mc <- peekChar
  case mc of
    Just '.' -> do
      t <- anyChar *> takeWhile1 isDigit
      pure $! parsePicos (fromIntegral real) t
    _ -> pure $! fromIntegral real
 where
  parsePicos :: Int64 -> B8.ByteString -> Pico
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where n  = max 0 (12 - B8.length t)
          t' = B8.foldl' (\a c -> 10 * a + fromIntegral (ord c .&. 15)) a0
                         (B8.take 12 t)

nominalDiffTime :: Parser NominalDiffTime
nominalDiffTime = do
    (h, m, s) <- pClockTime
    pure . fromRational . toRational $ s + 60*(fromIntegral m) + 60*60*(fromIntegral h)


-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
pClockTime :: Parser (Int, Int, Pico)
pClockTime = do
    h <- try $ signed decimal <* char ':'
    m <- try $ twoDigits <* char ':'
    s <- try seconds
    pure (h,m,s)
