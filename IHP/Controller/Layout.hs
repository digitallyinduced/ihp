{-|
Module: IHP.Controller.Layout
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Layout
( setLayout
, getLayout
, ViewLayout (..)
) where

import IHP.Prelude
import IHP.ViewSupport
import IHP.Controller.Context

newtype ViewLayout = ViewLayout ((?context :: ControllerContext) => Layout)

setLayout :: (?context :: ControllerContext) => ((?context :: ControllerContext) => Layout) -> IO ()
setLayout layout = do
    putContext (ViewLayout layout)
{-# INLINE setLayout #-}

getLayout :: (?context :: ControllerContext) => IO Layout
getLayout = do
    (ViewLayout layout) <- fromContext
    pure layout