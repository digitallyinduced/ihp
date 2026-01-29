{-|
Module: IHP.Postgres.Inet
Description: Adds support for storing IP addresses in INET fields. CIDR Notation is not supported at the moment.
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Postgres.Inet where

import BasicPrelude

import Data.Attoparsec.ByteString.Char8 as Attoparsec

-- We use the @ip@ package for representing IP addresses
import qualified Net.IP as IP
import Net.IP (IP)
import qualified Data.Text.Encoding as Text

-- | Parse an IP address from a postgres bytesting representation
parseIP :: ByteString -> Either String IP
parseIP bs = case parseOnly parser bs of
    Left err -> Left err
    Right val -> Right val
  where
    parser = do
        ip <- Attoparsec.takeWhile (\char -> char /= ' ')
        case IP.decode (Text.decodeUtf8 ip) of
            Just ip -> pure ip
            Nothing -> fail "Invalid IP"
