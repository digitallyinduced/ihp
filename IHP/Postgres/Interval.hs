{-|
Module: IHP.Postgres.Interval
Description: Adds support for the Postgres Interval type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Interval where

import GHC.Float
import ClassyPrelude hiding (try, map, (.))

import Data.Time.Clock
import Data.Fixed
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Time.Internal (getTimeOfDay)
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)

import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

import IHP.Postgres.TimeParser (pClockTime)

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space
    space1
    (Lexer.skipLineComment "//")
    (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

symbol' :: Text -> Parser Text
symbol' = Lexer.symbol' spaceConsumer



-- | See https://stackoverflow.com/questions/32398878/converting-postgres-interval-to-haskell-nominaltimediff-with-postgresql-simple
-- To support NominalDiffTime we parse Y year[s] M mon[s] D day[s] [-]HHH:MM:SS.[SSSs]
-- The default is the postgres format.
-- Corresponds to the postgresql interval 6 months see the documentation (https://www.postgresql.org/docs/current/datatype-datetime.html).

type Parser = Parsec Void Text

instance FromField NominalDiffTime where
    fromField f mdat =
        if typeOid f /= typoid pClockTime
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
    years <- try $ Lexer.decimal <* (choice $ map symbol' ["years", "year"])
    mons  <-  try $ Lexer.decimal <* (choice $ map symbol' ["mons", "mon"])
    days <-  try $ Lexer.decimal <* (choice $ map symbol' ["days", "day"])
    pure (years, mons, days)
