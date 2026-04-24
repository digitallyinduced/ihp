{-|
Module: IHP.Router.DSL
Description: Public API for the explicit-routes DSL

Re-exports the 'routes' quasi-quoter from "IHP.Router.DSL.TH" and the
'UrlCapture' class from "IHP.Router.Capture" so that modules defining
routes only need a single import.
-}
module IHP.Router.DSL
    ( routes
    , UrlCapture (..)
    , Segment (..)
    ) where

import IHP.Router.DSL.TH (routes)
import IHP.Router.Capture (UrlCapture (..), Segment (..))
