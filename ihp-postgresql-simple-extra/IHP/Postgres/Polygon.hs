{-|
Module: IHP.Postgres.Polygon
Description: Adds support for the Postgres Polygon type
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.Postgres.Polygon where

import BasicPrelude

import Data.Attoparsec.ByteString.Char8 hiding (Result, char8, Parser(..))
import Data.Attoparsec.Internal.Types (Parser)
import IHP.Postgres.Point
import qualified Data.Text as Text

-- | Represents a Postgres Polygon
--
-- See https://www.postgresql.org/docs/9.5/datatype-geometric.html
data Polygon = Polygon { points :: [Point] }
    deriving (Eq, Show, Ord)

parsePolygon :: Parser ByteString Polygon
parsePolygon = do
    string "("
    points <- parsePoint `sepBy` (char ',')
    string ")"
    pure $ Polygon points

-- | Serialize a Polygon to its Postgres text representation
polygonToText :: Polygon -> Text
polygonToText Polygon { points } =
    "(" <> Text.intercalate "," (map pointToText points) <> ")"
