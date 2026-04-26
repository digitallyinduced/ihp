{-# OPTIONS_HADDOCK not-home, hide #-}
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
, module IHP.Router.TypedRoute
, module Network.HTTP.Types.Method
, routes
, UrlCapture (..)
, Segment (..)
  -- Names the @[routes|...|]@ splice emits into user code. The emitted
  -- type signature for the lowercase-header binding references
  -- 'Controller', 'InitControllerContext', 'Request', and 'Respond' as
  -- constraint classes. They must be in scope at the splice call site.
, Controller (..)
, InitControllerContext
, Request
, Respond
, SwaggerUiController (..)
)
where

import Data.Attoparsec.ByteString.Char8
import IHP.RouterSupport
import ClassyPrelude hiding (index, delete, show, take, takeWhile, try)
import Data.String.Conversions (cs)
import IHP.ModelSupport (Id, Id' (..))
import IHP.Router.TypedRoute
import Network.HTTP.Types.Method (StdMethod (..))
import IHP.Router.Capture (UrlCapture (..), Segment (..))
import IHP.Router.IHP (routes)
import IHP.ControllerSupport (Controller (..), InitControllerContext, Respond)
import Network.Wai (Request)
import IHP.OpenApiSupport (SwaggerUiController (..))
