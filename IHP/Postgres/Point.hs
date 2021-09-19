{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.Point
Description: Adds support for the Postgres Point type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Point where

import GHC.Float
import BasicPrelude

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)
import           Data.Attoparsec.ByteString.Char8 hiding (Result, char8, Parser(..))
import Data.Attoparsec.Internal.Types (Parser)

-- | Represents a Postgres Point
--
-- See https://www.postgresql.org/docs/9.5/datatype-geometric.html
data Point = Point { x :: Double, y :: Double }
    deriving (Eq, Show, Ord)

instance FromField Point where
    fromField f v =
        if typeOid f /= $(inlineTypoid TI.point)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parsePoint bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val

parsePoint :: Parser ByteString Point
parsePoint = do
    string "("
    x <- doubleOrNaN
    string ","
    y <- doubleOrNaN
    string ")"
    pure $ Point { x, y }
        where
            -- Postgres supports storing NaN inside a point, so we have to deal
            -- with that here as well
            doubleOrNaN = double <|> (string "NaN" >> (pure $ 0 / 0))


instance ToField Point where
    toField = serializePoint

serializePoint :: Point -> Action
serializePoint Point { x, y } = Many
    [ Plain (byteString "point(")
    , toField x
    , Plain (char8 ',')
    , toField y
    , Plain (char8 ')')
    ]
