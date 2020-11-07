{-|
Module: IHP.Controller.Layout
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Layout
( setLayout
, ViewLayout (..)
) where

import IHP.Prelude
import IHP.ViewSupport
import IHP.Controller.Context
import qualified Data.TMap as TypeMap

newtype ViewLayout = ViewLayout ((?context :: ControllerContext) => Layout)

setLayout :: (?context :: ControllerContext) => ((?context :: ControllerContext) => Layout) -> IO ()
setLayout layout = do
    putContext (ViewLayout layout)
