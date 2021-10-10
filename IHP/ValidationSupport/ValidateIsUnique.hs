module IHP.ValidationSupport.ValidateIsUnique
( validateIsUnique
, validateIsUniqueCaseInsensitive
, withCustomErrorMessageIO
) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField
import IHP.ModelSupport
import IHP.ValidationSupport.Types
import IHP.QueryBuilder
import IHP.Fetch

-- | Validates that e.g. an email (or another field) is unique across all users before inserting.
--
-- This validator reads the given field name (e.g. email) from the record, and runs a database query
-- to check that there is no other record using the same field value (e.g. email value).
--
-- __Example:__ Validate that an email is unique
--
-- > action CreateUserAction = do
-- >     let user = newRecord @User
-- >     user
-- >         |> fill @'["email"]
-- >         |> validateIsUnique #email
-- >         >>= ifValid \case
-- >             Left user -> render NewView { .. }
-- >             Right user -> do
-- >                 createRecord user
-- >                 redirectTo UsersAction
validateIsUnique :: forall field model savedModel fieldValue modelId savedModelId. (
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
        , GetModelByTableName (GetTableName savedModel) ~ savedModel
        , Table savedModel
    ) => Proxy field -> model -> IO model
validateIsUnique fieldProxy model = validateIsUniqueCaseAware fieldProxy model True
{-# INLINE validateIsUnique #-}


-- | Case insensitive version of 'validateIsUnique'.
--
-- Uses a comparison like @LOWER(field) = LOWER(value)@ internally, so it's best to have an index for @LOWER(field)@ in your Schema.sql
--
-- >>> CREATE UNIQUE INDEX users_email_index ON users ((LOWER(email)));
--
-- __Example:__ Validate that an email is unique, ignoring case
--
-- > action CreateUserAction = do
-- >     let user = newRecord @User
-- >     user
-- >         |> fill @'["email"]
-- >         |> validateIsUniqueCaseInsensitive #email
-- >         >>= ifValid \case
-- >             Left user -> render NewView { .. }
-- >             Right user -> do
-- >                 createRecord user
-- >                 redirectTo UsersAction
validateIsUniqueCaseInsensitive :: forall field model savedModel fieldValue modelId savedModelId. (
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
        , GetModelByTableName (GetTableName savedModel) ~ savedModel
        , Table savedModel
    ) => Proxy field -> model -> IO model
validateIsUniqueCaseInsensitive fieldProxy model = validateIsUniqueCaseAware fieldProxy model False
{-# INLINE validateIsUniqueCaseInsensitive #-}

-- | Internal helper for 'validateIsUnique' and 'validateIsUniqueCaseInsensitive'
validateIsUniqueCaseAware :: forall field model savedModel fieldValue modelId savedModelId. (
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
        , GetModelByTableName (GetTableName savedModel) ~ savedModel
        , Table savedModel
    ) => Proxy field -> model -> Bool -> IO model
validateIsUniqueCaseAware fieldProxy model caseSensitive = do
    let value = getField @field model
    result <- query @savedModel
        |> (if caseSensitive
                then filterWhere (fieldProxy, value)
                else filterWhereCaseInsensitive (fieldProxy, value)
            )
        |> fetchOneOrNothing
    case result of
        Just value | not $ (getField @"id" model) == (getField @"id" value) -> pure (attachValidatorResult fieldProxy (Failure "This is already in use") model)
        _ -> pure (attachValidatorResult fieldProxy Success model)
{-# INLINE validateIsUniqueCaseAware #-}

-- | Overrides the error message of a given IO validator function.
--
-- __Example:__ Validate that an email is unique with a custom error message
--
-- > action CreateUserAction = do
-- >     let user = newRecord @User
-- >     user
-- >         |> fill @'["email"]
-- >         |> withCustomErrorMessageIO "Email Has Already Been Used" validateIsUnique #email
-- >         >>= ifValid \case
-- >             Left user -> render NewView { .. }
-- >             Right user -> do
-- >                 createRecord user
-- >                 redirectTo UsersAction
withCustomErrorMessageIO :: forall field model savedModel fieldValue modelId savedModelId. (
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
        , GetModelByTableName (GetTableName savedModel) ~ savedModel
    ) => Text -> (Proxy field -> model -> IO model) -> Proxy field -> model -> IO model
withCustomErrorMessageIO message validator field model = do
    validator field model >>= (\model ->
                                let maybeFailure = getValidationFailure field model
                                in case maybeFailure of
                                    Just _ -> pure $ attachFailure field message model
                                    Nothing -> pure model)
{-# INLINABLE withCustomErrorMessageIO #-}
