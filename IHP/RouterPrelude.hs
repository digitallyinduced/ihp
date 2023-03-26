{-|
Module: IHP.RouterPrelude
Description: Prelude used by modules like Web.Routes, Admin.Routes, etc.
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.RouterPrelude
( module IHP.RouterSupport
, module Data.Attoparsec.ByteString.Char8
, module ClassyPrelude
, module Data.String.Conversions
, module IHP.ModelSupport
, module Network.HTTP.Types.Method
)
where

import Data.Attoparsec.ByteString.Char8
import IHP.RouterSupport
import ClassyPrelude hiding (index, delete, show, take, takeWhile, try)
import Data.String.Conversions (cs)
import IHP.ModelSupport (Id, Id' (..))
import Network.HTTP.Types.Method (StdMethod (..))
