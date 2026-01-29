{-|
Module: IHP.Controller.Layout
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Layout
( setLayout
, getLayout
, ViewLayout (..)
) where

import Prelude
import IHP.ViewSupport
import IHP.Controller.Context
import Network.Wai (Request)

newtype ViewLayout = ViewLayout ((?context :: ControllerContext, ?request :: Request) => Layout)

setLayout :: (?context :: ControllerContext) => ((?context :: ControllerContext, ?request :: Request) => Layout) -> IO ()
setLayout layout = do
    putContext (ViewLayout layout)
{-# INLINE setLayout #-}

getLayout :: (?context :: ControllerContext, ?request :: Request) => IO Layout
getLayout = do
    (ViewLayout layout) <- fromContext
    pure layout