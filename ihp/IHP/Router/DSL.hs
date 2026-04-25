{-|
Module: IHP.Router.DSL
Description: Public API for the IHP-flavoured explicit-routes DSL

Re-exports the IHP-flavoured 'routes' quasi-quoter from "IHP.Router.IHP"
and the 'UrlCapture' class from "IHP.Router.Capture" so that modules
defining routes only need a single import.

Plain WAI users (no IHP dependency) should import "IHP.Router.WAI"
from the @ihp-router@ package instead — that gives the IHP-free flavour
of the same quoter (no 'CanRoute' instance, no @webRoutes@ binding).
-}
module IHP.Router.DSL
    ( routes
    , UrlCapture (..)
    , Segment (..)
    ) where

import IHP.Router.IHP (routes)
import IHP.Router.Capture (UrlCapture (..), Segment (..))
