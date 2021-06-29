{-|
Module: Test.Postgres.Support
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.Support where

import IHP.Prelude
import Data.ByteString.Builder (toLazyByteString)
import Database.PostgreSQL.Simple.ToField

instance Eq Action where
    (==) (Many a) (Many b) = a == b
    (==) (Plain a) (Plain b) = toLazyByteString a == toLazyByteString b
