{-# OPTIONS_HADDOCK not-home, hide #-}
module IHP.TypedControllerPrelude
    ( module IHP.ControllerPrelude
    , module IHP.Controller.TypedAction
    ) where

import IHP.ControllerPrelude hiding (action, beforeAction)
import IHP.Controller.TypedAction
