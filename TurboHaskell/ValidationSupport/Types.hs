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
import GHC.Records
import Data.Proxy
import GHC.TypeLits (KnownSymbol, Symbol)
import Control.Lens hiding ((|>))
import Data.Generics.Product hiding (getField, HasField)
import Data.Generics.Product.Types
import GHC.Generics
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.UUID
import Data.Default
import Control.Monad.State
import Data.Default
import Unsafe.Coerce
import Data.Dynamic (Dynamic, toDyn)

data ValidatorResult = Success | Failure Text deriving (Show, Eq, Generic)

isSuccess Success = True
isSuccess _       = False

modelToEither :: forall model result. (?model :: model, HasTypes result ValidatorResult) => result -> Either result model
modelToEither result =
    let
        validatorResults = toListOf (types @ValidatorResult) result
        failures = filter isFailure validatorResults
    in
        case failures of
            [] -> Right ?model
            _  -> Left result

isFailure Failure {} = True
isFailure otherwise  = False

type family ValidatorResultFor model

type ValidationMonad model = (?model :: model) => StateT (ValidatorResultFor model) IO ()
validateRecord' :: forall model controllerContext. (
        ?controllerContext :: controllerContext
        , Default (ValidatorResultFor model)
        , Data.Generics.Product.Types.HasTypes (ValidatorResultFor model) ValidatorResult
        , Typeable model
        , Typeable (ValidatorResultFor model)
        , HasField "validations" controllerContext (IORef [Dynamic])
    ) => ValidationMonad model -> model -> IO (Either (ValidatorResultFor model) model)
validateRecord' arg model = do
    let ?model = model
    result <- runStateT (do arg; state <- get; return state) (def :: ValidatorResultFor model)
    modifyIORef (getField @"validations" ?controllerContext) (\validations -> (toDyn (model, fst result)):validations)
    return (modelToEither $ fst result)

class ValidateRecord record controllerContext where
    validateRecord :: (?modelContext :: ModelContext, ?controllerContext :: controllerContext, ?model :: record) => StateT (ValidatorResultFor record) IO ()
    validateRecord = return ()

