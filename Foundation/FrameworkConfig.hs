module Foundation.FrameworkConfig where

import ClassyPrelude
import Foundation.Environment

class FrameworkConfig where
	baseUrl :: Text
	environment :: Environment


data RootApplication = RootApplication deriving (Eq)