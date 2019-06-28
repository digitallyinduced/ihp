module TurboHaskell.FrameworkConfig where

import ClassyPrelude
import TurboHaskell.Environment
import TurboHaskell.ControllerSupport

class FrameworkConfig where
    baseUrl :: Text
    environment :: Environment


data RootApplication = RootApplication deriving (Eq)

instance Controller RootApplication () where
