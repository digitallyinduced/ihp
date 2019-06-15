module TurboHaskell.FrameworkConfig where

import ClassyPrelude
import TurboHaskell.Environment

class FrameworkConfig where
	baseUrl :: Text
	environment :: Environment


data RootApplication = RootApplication deriving (Eq)