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
import GHC.TypeLits (KnownSymbol, Symbol)
import Control.Lens hiding ((|>))
import Data.Generics.Product hiding (getField, HasField)
import Data.Generics.Product.Types
import GHC.Generics 
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.UUID
import Data.Default
import Control.Monad.State
import Unsafe.Coerce
import Data.Dynamic (Dynamic, toDyn)
import Data.Generics.Product

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
isFailure _  = False

type family ValidatorResultFor model

type ValidationMonad model = (?model :: model) => StateT (ValidatorResultFor model) IO ()
validateRecord' :: forall model controllerContext. (
        ?controllerContext :: controllerContext
        , Default (ValidatorResultFor model)
        , Data.Generics.Product.Types.HasTypes (ValidatorResultFor model) ValidatorResult
        , Typeable model
        , Typeable (ValidatorResultFor model)
        , Records.HasField "validations" controllerContext (IORef [Dynamic])
    ) => ValidationMonad model -> model -> IO (Either (ValidatorResultFor model) model)
validateRecord' arg model = do
    let ?model = model
    result <- runStateT (do arg; get) (def :: ValidatorResultFor model)
    modifyIORef (Records.getField @"validations" ?controllerContext) (\validations -> (toDyn (model, fst result)):validations)
    return (modelToEither $ fst result)

{-# INLINE attachFailure #-}
attachFailure :: forall validationState field. (KnownSymbol field, HasField field validationState validationState ValidatorResult ValidatorResult) => Proxy field -> Text -> StateT validationState IO ()
attachFailure field message = attachValidatorResult field (Failure message)

{-# INLINE attachValidatorResult #-}
attachValidatorResult :: forall validationState field. (KnownSymbol field, HasField field validationState validationState ValidatorResult ValidatorResult) => Proxy field -> ValidatorResult -> StateT validationState IO ()
attachValidatorResult _ validatorResult = do
    validationState <- get
    put (validationState & ((field @field) .~ validatorResult))