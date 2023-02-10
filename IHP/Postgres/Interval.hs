{-|
Module: IHP.Postgres.Interval
Description: Adds support for the Postgres Interval type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Interval where

import GHC.Float
import IHP.Prelude

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)

import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


-- | See https://stackoverflow.com/questions/32398878/converting-postgres-interval-to-haskell-nominaltimediff-with-postgresql-simple
-- To support NominalDiffTime we parse Y year[s] M mon[s] D day[s] [-]HHH:MM:SS.[SSSs]
-- The default is the postgres format.
-- Corresponds to the postgresql interval 6 months see the documentation (https://www.postgresql.org/docs/current/datatype-datetime.html).
instance FromField NominalDiffTime where
    fromField f mdat =
        if typeOid f /= typoid interval
            then returnError Incompatible f ""
            else case mdat of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> case parseOnly (pNominalDiffTime <* endOfInput) dat of
                    Left msg  -> returnError ConversionFailed f msg
                    Right t   -> return t

pNominalDiffTime :: Parser NominalDiffTime
pNominalDiffTime = do
    (years, mons, days) <- pCalTime
    (h, m, s) <- pClockTime

    let calTime =  fromRational . toRational $ (\[y,m,d] -> (365*nominalDay*y 30*nominalDay*m + nominalDay*d)) $ map (fromMaybe 0) [years, mons, days]
    let clockTime = fromRational . toRational $ s + 60*(fromIntegral m) + 60*60*(fromIntegral h)

    pure (calTime + clockTime)

-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
pCalTime :: Parser (Maybe Int, Maybe Int, Maybe Int)
pCalTime = do
    years <- try $ signed decimal <* choice $ map symbol' ["years", "year"]
    mons  <-  try $ signed decimal <* choice $ map symbol' ["mons", "mon"]
    days <-  try $ signed decimal <* choice $ map symbol' ["days", "day"]
    pure (years, mons, days)

-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
pClockTime :: Parser (Int, Int, Pico)
pClockTime = do
    h <- try $ signed decimal <* char ':'
    m <- try $ twoDigits <* char ':'
    s <- try seconds
    if m < 60 && s <= 60
        then return (h, m, s)
        else fail "invalid interval"
