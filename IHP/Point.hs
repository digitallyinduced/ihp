{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Point
Description: Adds support for the Postgres Point type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Point where

import GHC.Float
import BasicPrelude

import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder (byteString, char8)
import           Data.Attoparsec.ByteString.Char8 hiding (Result, char8)

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
                   case parseOnly parser bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val
      where
        parser = do
            string "("
            x <- double
            string ","
            y <- double
            string ")"
            pure $ Point { x, y }

instance ToField Point where
    toField Point { x, y } = Many $
        (Plain (byteString "point(")) :
        (toField x) :
        (Plain (char8 ',')) :
        (toField y) :
        [Plain (char8 ')')] 