module IHP.Postgres.TimeParser where

-- |
-- Module:      Database.PostgreSQL.Simple.Time.Internal.Parser
-- Copyright:   (c) 2012-2015 Leon P Smith
--              (c) 2015 Bryan O'Sullivan
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Parsers for parsing dates and times.


import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.ByteString.Char8 as A
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Fixed (Pico, Fixed(MkFixed))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar.Compat (Day, fromGregorianValid, addDays)
import Data.Time.Clock.Compat (UTCTime(..))
import Data.Time.Format.ISO8601.Compat (iso8601ParseM)
import Data.Time.LocalTime.Compat (CalendarDiffTime)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Time.LocalTime.Compat as Local

-- Take from Postgresql Internal to facilitate a definition of a NominalDiffTime FromField
toPico :: Integer -> Pico
toPico = MkFixed


-- | Parse a two-digit integer (e.g. day of month, hour).
twoDigits :: Parser Int
twoDigits = do
  a <- digit
  b <- digit
  let c2d c = ord c .&. 15
  return $! c2d a * 10 + c2d b


-- | Parse a time of the form @HH:MM[:SS[.SSS]]@.
timeOfDay :: Parser Local.TimeOfDay
timeOfDay = do
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
      return $! parsePicos (fromIntegral real) t
    _ -> return $! fromIntegral real
 where
  parsePicos :: Int64 -> B8.ByteString -> Pico
  parsePicos a0 t = toPico (fromIntegral (t' * 10^n))
    where n  = max 0 (12 - B8.length t)
          t' = B8.foldl' (\a c -> 10 * a + fromIntegral (ord c .&. 15)) a0
                         (B8.take 12 t)

-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
pClockTime :: Parser (Int, Int, Pico)
pClockTime = do
    h <- try $ signed decimal <* char ':'
    m <- try $ twoDigits <* char ':'
    s <- try seconds
    if m < 60 && s <= 60
        then return (h, m, s)
        else fail "invalid interval"
