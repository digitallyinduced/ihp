module TurboHaskell.ValidationSupport.ValidateIsUnique (validateIsUnique) where

import TurboHaskell.Prelude
import Database.PostgreSQL.Simple.ToField
import TurboHaskell.ModelSupport
import TurboHaskell.ValidationSupport.Types
import TurboHaskell.HaskellSupport
import TurboHaskell.QueryBuilder

{-# INLINE validateIsUnique #-}
validateIsUnique :: forall field model savedModel validationState fieldValue validationStateValue fetchedModel. (
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
    ) => Proxy field -> model -> IO model
validateIsUnique fieldProxy model = do
    let value = getField @field model
    result <- query @savedModel
        |> filterWhere (fieldProxy, value)
        |> fetchOneOrNothing
    case result of
        Nothing -> pure (attachValidatorResult fieldProxy Success model)
        Just value -> pure (attachValidatorResult fieldProxy (Failure "This is already in use") model)
