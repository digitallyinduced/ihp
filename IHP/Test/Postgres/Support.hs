{-|
Module: Test.Postgres.Support
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.Postgres.Support where

import IHP.Prelude
import Data.ByteString.Builder (toLazyByteString)
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Builder as Builder

deriving instance Eq Action

instance Eq Builder.Builder where
    a == b = (Builder.toLazyByteString a) == (Builder.toLazyByteString b)