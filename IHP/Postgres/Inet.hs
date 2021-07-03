{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.Inet
Description: Adds support for storing IP addresses in INET fields. CIDR Notation is not supported at the moment.
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Inet where

import BasicPrelude

import qualified Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import qualified Database.PostgreSQL.Simple.Types
import Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import Data.ByteString.Builder (byteString, char8)
import Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.String.Conversions (cs)

-- We use the @ip@ package for representing IP addresses
import qualified Net.IP as IP
import Net.IP (IP)

instance FromField IP where
    fromField f v =
        if typeOid f /= $(inlineTypoid TI.inet)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parser bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val
      where
        parser = do
            ip <- Attoparsec.takeWhile (\char -> char /= ' ')
            case IP.decode (cs ip) of
                Just ip -> pure ip
                Nothing -> fail "Invalid IP"

instance ToField IP where
    toField ip = toField (IP.encode ip)
