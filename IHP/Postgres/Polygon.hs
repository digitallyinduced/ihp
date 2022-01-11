{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.Polygon
Description: Adds support for the Postgres Polygon type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Polygon where

import GHC.Float
import BasicPrelude

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)
import           Data.Attoparsec.ByteString.Char8 hiding (Result, char8, Parser(..))
import Data.Attoparsec.Internal.Types (Parser)
import IHP.Postgres.Point

-- | Represents a Postgres Polygon
--
-- See https://www.postgresql.org/docs/9.5/datatype-geometric.html
data Polygon = Polygon { points :: [Point] }
    deriving (Eq, Show, Ord)

instance FromField Polygon where
    fromField f v =
        if typeOid f /= $(inlineTypoid TI.polygon)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parsePolygon bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val

parsePolygon :: Parser ByteString Polygon
parsePolygon = do
    string "("
    points <- parsePoint `sepBy` (char ',')
    string ")"
    pure $ Polygon points

instance ToField Polygon where
    toField = serializePolygon

serializePolygon :: Polygon -> Action
serializePolygon Polygon { points } = Many $
    (Plain (byteString "polygon'")):
    ( (intersperse (Plain $ char8 ',') $ map serializePoint' points)
      ++ [ Plain (char8 '\'') ])
    where
        serializePoint' :: Point -> Action
        serializePoint' Point { x, y } = Many $
            [ Plain (char8 '(')
            , toField x
            , Plain (char8 ',')
            , toField y
            , Plain (char8 ')')
            ]
