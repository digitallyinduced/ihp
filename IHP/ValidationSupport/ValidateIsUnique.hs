module IHP.ValidationSupport.ValidateIsUnique (validateIsUnique) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField
import IHP.ModelSupport
import IHP.ValidationSupport.Types
import IHP.HaskellSupport
import IHP.QueryBuilder

-- | Validates that e.g. an email (or another field) is unique across all users before inserting.
--
-- This validator reads the given field name (e.g. email) from the record, and runs a database query
-- to check that there is no other record using the same field value (e.g. email value).
--
-- __Example:__ Validate that an email is unique
--
-- > action CreateUserAction = do
-- >     let user = newRecord @NewUser
-- >     user
-- >         |> fill @'["email"]
-- >         |> validateIsUnique #email
-- >         >>= ifValid \case
-- >             Left user -> render NewView { .. } 
-- >             Right user -> do
-- >                 createRecord user
-- >                 redirectTo UsersAction
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
{-# INLINE validateIsUnique #-}