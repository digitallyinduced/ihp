{-|
Module: IHP.Postgres.Interval
Description: Adds support for the Postgres Interval type
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Interval where

import BasicPrelude

import Data.Aeson
import Data.Attoparsec.ByteString.Char8 as Attoparsec

import IHP.Postgres.TimeParser (PGInterval(..))
import qualified Data.Text.Encoding as Text

pPGInterval = do
    bs <- takeByteString
    pure (PGInterval bs)

instance FromJSON PGInterval where
     parseJSON = withText "PGInterval" $ \text -> pure (PGInterval (encodeUtf8 text))

instance ToJSON PGInterval where
    toJSON (PGInterval pgInterval) = String (Text.decodeUtf8 pgInterval)
