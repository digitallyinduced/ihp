{-|
Module: IHP.Postgres.Point
Description: Adds support for the Postgres Point type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Point where

import GHC.Float
import BasicPrelude

import Data.Attoparsec.ByteString.Char8 hiding (Result, char8, Parser(..))
import Data.Attoparsec.Internal.Types (Parser)
import Data.Aeson

-- | Represents a Postgres Point
--
-- See https://www.postgresql.org/docs/9.5/datatype-geometric.html
data Point = Point { x :: Double, y :: Double }
    deriving (Eq, Show, Ord)

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

-- | Serialize a Point to its Postgres text representation
pointToText :: Point -> Text
pointToText Point { x, y } = "(" <> tshow x <> "," <> tshow y <> ")"

instance FromJSON Point where
    parseJSON = withObject "Point" $ \v -> Point
        <$> v .: "x"
        <*> v .: "y"

instance ToJSON Point where
    toJSON Point { x, y } = object [ "x" .= x, "y" .= y ]
