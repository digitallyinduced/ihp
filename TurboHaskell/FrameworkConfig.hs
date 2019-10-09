module TurboHaskell.FrameworkConfig where

import ClassyPrelude
import TurboHaskell.Environment
import TurboHaskell.ControllerSupport
import TurboHaskell.RouterSupport

class FrameworkConfig where
    baseUrl :: Text
    environment :: Environment


data RootApplication = RootApplication deriving (Eq, Show)

instance Controller RootApplication where

instance FrontControllerPrefix RootApplication where
    prefix = ""
