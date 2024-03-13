module Main where

import Prelude
import Main.Utf8 (withUtf8)
import qualified Web.ClientSession as ClientSession
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Base64 as Base64

-- Prints a private key to be used as the IHP_SESSION_SECRET
main :: IO ()
main = withUtf8 do
    (string, _) <- ClientSession.randomKey
    let encoded = Base64.encode string
    ByteString.putStr encoded