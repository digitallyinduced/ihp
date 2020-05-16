{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, AllowAmbiguousTypes #-}

module IHP.FetchRelated (fetchRelated, collectionFetchRelated, fetchRelatedOrNothing, maybeFetchRelatedOrNothing) where

import IHP.Prelude
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import IHP.ModelSupport (GetTableName, ModelContext, GetModelById, Include)
import IHP.QueryBuilder

collectionFetchRelated :: forall model relatedField relatedFieldValue relatedModel. (
        ?modelContext :: ModelContext,
        HasField relatedField model relatedFieldValue,
        HasField "id" relatedModel relatedFieldValue,
        UpdateField relatedField model (Include relatedField model) relatedFieldValue relatedModel,
        Fetchable relatedFieldValue relatedModel,
        KnownSymbol (GetTableName relatedModel),
        PG.FromRow relatedModel,
        HasField relatedField model relatedFieldValue,
        Eq relatedFieldValue,
        ToField relatedFieldValue,
        KnownSymbol relatedField,
        HasField "id" relatedModel relatedFieldValue,
        Show relatedFieldValue
    ) => Proxy relatedField -> [model] -> IO [Include relatedField model]
collectionFetchRelated relatedField model = do
    relatedModels :: [relatedModel] <- query @relatedModel |> filterWhereIn (#id, map (getField @relatedField) model) |> fetch
    let
        assignRelated :: model -> Include relatedField model
        assignRelated model =
            let
                relatedModel :: relatedModel
                relatedModel = case find (\r -> (getField @"id" r :: relatedFieldValue) == targetForeignKey) relatedModels of
                        Just m -> m
                        Nothing -> error ("Could not find record with id = " <> show targetForeignKey <> " in result set. Looks like the foreign key is pointing to a non existing record")
                targetForeignKey = (getField @relatedField model :: relatedFieldValue)
            in
                updateField @relatedField relatedModel model

    let
        result :: [Include relatedField model]
        result = map assignRelated model
    pure result

fetchRelated :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) fieldValue (FetchResult fieldValue fetchModel),
        HasField field model fieldValue,
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel
    ) => Proxy field -> model -> IO (Include field model)
fetchRelated relatedField model = do
    result :: FetchResult fieldValue fetchModel <- fetch ((getField @field model) :: fieldValue)
    let model' = updateField @field result model
    pure model'

fetchRelatedOrNothing :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) (Maybe fieldValue) (Maybe (FetchResult fieldValue fetchModel)),
        HasField field model (Maybe fieldValue),
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel
    ) => Proxy field -> model -> IO (Include field model)
fetchRelatedOrNothing relatedField model = do
    result :: Maybe (FetchResult fieldValue fetchModel) <- case getField @field model of
            Just fieldValue -> Just <$> fetch fieldValue
            Nothing -> pure Nothing
    let model' = updateField @field result model
    pure model'

maybeFetchRelatedOrNothing :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) (Maybe fieldValue) (Maybe (FetchResult fieldValue fetchModel)),
        HasField field model (Maybe fieldValue),
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel
    ) => Proxy field -> Maybe model -> IO (Maybe (Include field model))
maybeFetchRelatedOrNothing relatedField = maybe (pure Nothing) (\q -> fetchRelatedOrNothing relatedField q >>= pure . Just)
