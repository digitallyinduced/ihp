module TurboHaskell.ValidationSupport.ValidateIsUnique (validateIsUnique) where

import TurboHaskell.Prelude
import Database.PostgreSQL.Simple.ToField
import TurboHaskell.ModelSupport
import TurboHaskell.ValidationSupport.Types
import TurboHaskell.HaskellSupport
import TurboHaskell.QueryBuilder

{-# INLINE validateIsUnique #-}
validateIsUnique :: forall field model savedModel validationState fieldValue validationStateValue fetchedModel modelId savedModelId. (
        savedModel ~ NormalizeModel model
        , ?modelContext :: ModelContext
        , FromRow savedModel
        , KnownSymbol field
        , HasField field model fieldValue
        , HasField field savedModel fieldValue
        , KnownSymbol (GetTableName savedModel)
        , ToField fieldValue
        , EqOrIsOperator fieldValue
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
        , HasField "id" savedModel savedModelId
        , HasField "id" model modelId
        , savedModelId ~ modelId
        , Eq modelId
    ) => Proxy field -> model -> IO model
validateIsUnique fieldProxy model = do
    let value = getField @field model
    result <- query @savedModel
        |> filterWhere (fieldProxy, value)
        |> fetchOneOrNothing
    case result of
        Just value | not $ (getField @"id" model) == (getField @"id" value) -> pure (attachValidatorResult fieldProxy (Failure "This is already in use") model)
        _ -> pure (attachValidatorResult fieldProxy Success model)
