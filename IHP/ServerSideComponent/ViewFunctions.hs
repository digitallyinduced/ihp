{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.ServerSideComponent.ViewFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.ViewFunctions where

import IHP.Prelude
import IHP.ViewSupport
import IHP.ServerSideComponent.Types
import IHP.HtmlSupport.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context

import qualified Data.Typeable as Typeable

component :: forall component action. (Component component action, Typeable component) => Html
component = [hsx|<div class="ihp-ssc" data-url={url}>{inner}</div>|]
    where
        inner = render (initialState @component)
        url = "ws://localhost:8000/SSC/" <> typeName
        typeName = (undefined :: component)
                |> Typeable.typeOf
                |> show