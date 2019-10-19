{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module TurboHaskell.ValidationSupport.Types where

import           ClassyPrelude
import           TurboHaskell.ModelSupport

import           TurboHaskell.QueryBuilder (fetchOneOrNothing, Fetchable)
import TurboHaskell.AuthSupport.Authorization
import qualified Data.Text as Text
import TurboHaskell.NameSupport (humanize)
import qualified GHC.Records as Records
import Data.Proxy
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.UUID
import Data.Default
import Control.Monad.State.Strict
import Unsafe.Coerce
import Data.Dynamic (Dynamic, toDyn)

data ValidatorResult = Success | Failure Text deriving (Show, Eq)

isSuccess Success = True
isSuccess _       = False

isFailure Failure {} = True
isFailure _  = False

type family ValidatorResultFor model

{-# INLINE attachFailure #-}
attachFailure :: (KnownSymbol field) => Proxy field -> Text -> StateT [(Text, Text)] IO ()
attachFailure field message = do
    validationState <- get
    put ((Text.pack (symbolVal field), message):validationState)

newtype ValidationResults = ValidationResults (IORef [Dynamic])