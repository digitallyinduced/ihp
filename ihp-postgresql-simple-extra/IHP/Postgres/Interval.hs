{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.Interval
Description: Adds support for the Postgres Interval type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Interval where

import BasicPrelude

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Aeson

import IHP.Postgres.TimeParser (PGInterval(..))
import qualified Data.Text.Encoding as Text

instance FromField PGInterval where
    fromField f v =
        if typeOid f /= $(inlineTypoid TI.interval)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs -> case parseOnly pPGInterval bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val

pPGInterval = do
    bs <- takeByteString
    pure (PGInterval bs)

instance ToField PGInterval where
    toField (PGInterval interval) = toField (interval)

instance FromJSON PGInterval where
     parseJSON = withText "PGInterval" $ \text -> pure (PGInterval (encodeUtf8 text))

instance ToJSON PGInterval where
    toJSON (PGInterval pgInterval) = String (Text.decodeUtf8 pgInterval)
