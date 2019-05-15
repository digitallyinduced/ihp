module Foundation.RouterPrelude
	( module Foundation.RouterSupport
	, module Data.Attoparsec.Char8
	, module ClassyPrelude
	, module Data.String.Conversions
	, module Foundation.ModelSupport
	)
where

import Data.Attoparsec.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput)
import Foundation.RouterSupport
import ClassyPrelude hiding (index, delete, show, take)
import Data.String.Conversions (cs)
import Foundation.ModelSupport (Id, Id' (..))