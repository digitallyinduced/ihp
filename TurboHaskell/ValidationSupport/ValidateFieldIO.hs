module TurboHaskell.ValidationSupport.ValidateFieldIO (validateFieldIO) where

import           ClassyPrelude
import           Data.Proxy
import qualified Data.Text                            as Text
import qualified Data.UUID
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import           TurboHaskell.AuthSupport.Authorization
import           TurboHaskell.ModelSupport
import           TurboHaskell.NameSupport               (humanize)
import           TurboHaskell.ValidationSupport.Types
import           GHC.Records
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State.Strict
import TurboHaskell.HaskellSupport
import TurboHaskell.QueryBuilder

type CustomIOValidation value = value -> IO ValidatorResult

{-# INLINE validateFieldIO #-}
validateFieldIO :: forall field model savedModel idType validationState fieldValue validationStateValue fetchedModel. (
        savedModel ~ NormalizeModel model
        , ?modelContext :: ModelContext
        , PG.FromRow savedModel
        , KnownSymbol field
        , HasField field model fieldValue
        , KnownSymbol (GetTableName savedModel)
        , PG.ToField fieldValue
        , EqOrIsOperator fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
    ) => Proxy field -> CustomIOValidation fieldValue -> model -> IO model
validateFieldIO fieldProxy customValidation model = do
    let value :: fieldValue = getField @field model
    result <- liftIO (customValidation value)
    pure (attachValidatorResult fieldProxy result model)
