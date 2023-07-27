{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.ServerSideComponent.ViewFunctions
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.ServerSideComponent.ViewFunctions where

import IHP.Prelude
import IHP.ViewSupport
import IHP.ServerSideComponent.Types
import IHP.HSX.QQ (hsx)
import qualified Data.Aeson as Aeson

import qualified Data.Typeable as Typeable

component :: forall component action. (Component component action, Typeable component, Aeson.ToJSON component) => Html
component = componentFromState (initialState @component)

componentFromState :: forall component action. (Component component action, Typeable component, Aeson.ToJSON component) => component -> Html
componentFromState state = [hsx|<div class="ihp-ssc" data-path={path} data-initial-state={Aeson.encode state}>{render state}</div>|]
    where
        path = "/SSC/" <> typeName
        typeName = (undefined :: component)
                |> Typeable.typeOf
                |> show
