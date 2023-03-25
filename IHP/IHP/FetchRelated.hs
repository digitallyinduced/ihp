{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, AllowAmbiguousTypes #-}
{-|
Module: IHP.FetchRelated
Description:  Provides fetchRelated, collectionFetchRelated, etc.
Copyright: (c) digitally induced GmbH, 2020

This modules provides helper functions to access relationshops for a model.

See https://ihp.digitallyinduced.com/Guide/relationships.html for some examples.
-}
module IHP.FetchRelated (fetchRelated, collectionFetchRelated, collectionFetchRelatedOrNothing, fetchRelatedOrNothing, maybeFetchRelatedOrNothing) where

import IHP.Prelude
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple as PG
import IHP.ModelSupport (Include, Id', PrimaryKey, GetModelByTableName, Table)
import IHP.QueryBuilder
import IHP.Fetch

-- | This class provides the collectionFetchRelated function
--
-- This function is provided by this class as we have to deal with two cases:
-- 
-- 1. the related field is a id, e.g. like the company ids in @users |> collectionFetchRelated #companyId@
-- 2. the related field is a query builder, e.g. in @posts |> collectionFetchRelated #comments@
class CollectionFetchRelated relatedFieldValue relatedModel where
    collectionFetchRelated :: forall model relatedField. (
            ?modelContext :: ModelContext,
            HasField relatedField model relatedFieldValue,
            UpdateField relatedField model (Include relatedField model) relatedFieldValue (FetchResult relatedFieldValue relatedModel),
            Fetchable relatedFieldValue relatedModel,
            KnownSymbol (GetTableName relatedModel),
            PG.FromRow relatedModel,
            KnownSymbol relatedField
        ) => Proxy relatedField -> [model] -> IO [Include relatedField model]

-- | This class provides the collectionFetchRelatedOrNothing function
--
-- This function is provided by this class as we have to deal with two cases:
-- 
-- 1. the related field is an id, e.g. like the company ids in @users |> collectionFetchRelated #companyId@
-- 2. the related field is a query builder, e.g. in @posts |> collectionFetchRelated #comments@
class CollectionFetchRelatedOrNothing relatedFieldValue relatedModel where
    collectionFetchRelatedOrNothing :: forall model relatedField. (
            ?modelContext :: ModelContext,
            HasField relatedField model (Maybe relatedFieldValue),
            UpdateField relatedField model (Include relatedField model) (Maybe relatedFieldValue) (Maybe (FetchResult relatedFieldValue relatedModel)),
            Fetchable relatedFieldValue relatedModel,
            KnownSymbol (GetTableName relatedModel),
            PG.FromRow relatedModel,
            KnownSymbol relatedField
        ) => Proxy relatedField -> [model] -> IO [Include relatedField model]

-- | Provides collectionFetchRelated for ids, e.g. @collectionFetchRelated #companyId@
--
-- When we want to fetch all the users with their companies, we can use collectionFetchRelated like this:
--
-- > users <- query @User
-- >     |> fetch
-- >     >>= collectionFetchRelated #companyId
--
-- This will query all users with their company. The type of @users@ is @[Include "companyId" User]@.
--
-- This example will trigger only two SQL queries:
-- 
-- > SELECT * FROM users
-- > SELECT * FROM companies WHERE id IN (?)
instance (
        Eq (PrimaryKey tableName)
        , ToField (PrimaryKey tableName)
        , Show (PrimaryKey tableName)
        , HasField "id" relatedModel (Id' tableName)
        , relatedModel ~ GetModelByTableName (GetTableName relatedModel)
        , Table relatedModel
        ) => CollectionFetchRelated (Id' tableName) relatedModel where
    collectionFetchRelated :: forall model relatedField. (
            ?modelContext :: ModelContext,
            HasField relatedField model (Id' tableName),
            UpdateField relatedField model (Include relatedField model) (Id' tableName) (FetchResult (Id' tableName) relatedModel),
            Fetchable (Id' tableName) relatedModel,
            KnownSymbol (GetTableName relatedModel),
            PG.FromRow relatedModel,
            KnownSymbol relatedField,
            Table relatedModel
        ) => Proxy relatedField -> [model] -> IO [Include relatedField model]
    collectionFetchRelated relatedField model = do
        relatedModels :: [relatedModel] <- query @relatedModel |> filterWhereIn (#id, map (getField @relatedField) model) |> fetch
        let
            assignRelated :: model -> Include relatedField model
            assignRelated model =
                let
                    relatedModel :: relatedModel
                    relatedModel = case find (\r -> (getField @"id" r :: Id' tableName) == targetForeignKey) relatedModels of
                            Just m -> m
                            Nothing -> error ("Could not find record with id = " <> show targetForeignKey <> " in result set. Looks like the foreign key is pointing to a non existing record")
                    targetForeignKey = (getField @relatedField model :: Id' tableName)
                in
                    updateField @relatedField relatedModel model

        let
            result :: [Include relatedField model]
            result = map assignRelated model
        pure result

-- | Provides collectionFetchRelatedOrNothing for nullable ids, e.g. @collectionFetchRelatedOrNothing #companyId@
--
-- When we want to fetch all the users with their companies, we can use collectionFetchRelatedOrNothing like this:
--
-- > users <- query @User
-- >     |> fetch
-- >     >>= collectionFetchRelatedOrNothing #companyId
--
-- This will query all users with their company. The type of @users@ is @[Include "companyId" User]@.
--
-- This example will trigger only two SQL queries:
-- 
-- > SELECT * FROM users
-- > SELECT * FROM companies WHERE id IN (?)
instance (
        Eq (PrimaryKey tableName)
        , ToField (PrimaryKey tableName)
        , HasField "id" relatedModel (Id' tableName)
        , relatedModel ~ GetModelByTableName (GetTableName relatedModel)
        , Table relatedModel
        ) => CollectionFetchRelatedOrNothing (Id' tableName) relatedModel where
    collectionFetchRelatedOrNothing :: forall model relatedField. (
            ?modelContext :: ModelContext,
            HasField relatedField model (Maybe (Id' tableName)),
            UpdateField relatedField model (Include relatedField model) (Maybe (Id' tableName)) (Maybe (FetchResult (Id' tableName) relatedModel)),
            Fetchable (Id' tableName) relatedModel,
            KnownSymbol (GetTableName relatedModel),
            PG.FromRow relatedModel,
            KnownSymbol relatedField
        ) => Proxy relatedField -> [model] -> IO [Include relatedField model]
    collectionFetchRelatedOrNothing relatedField model = do
        relatedModels :: [relatedModel] <- query @relatedModel |> filterWhereIn (#id, mapMaybe (getField @relatedField) model) |> fetch
        let
            assignRelated :: model -> Include relatedField model
            assignRelated model =
                let
                    relatedModel :: Maybe (FetchResult (Id' tableName) relatedModel)
                    relatedModel = find (\r -> Just (getField @"id" r :: (Id' tableName)) == targetForeignKey) relatedModels
                    targetForeignKey = (getField @relatedField model :: Maybe (Id' tableName))
                in
                    updateField @relatedField relatedModel model

        let
            result :: [Include relatedField model]
            result = map assignRelated model
        pure result

-- | Provides collectionFetchRelated for QueryBuilder's, e.g. @collectionFetchRelated #comments@
--
-- When we want to fetch all the comments for a list of posts, we can use collectionFetchRelated like this:
--
-- > posts <- query @Post
-- >     |> fetch
-- >     >>= collectionFetchRelated #comments
--
-- This will query all posts with their comments. The type of @posts@ is @[Include "comments" Post]@.
--
-- When fetching query builders, currently the implementation is not very efficient. E.g. given 10 Posts above, it will run 10 queries to fetch the comments. We should optimise this behavior in the future.
instance (relatedModel ~ GetModelByTableName relatedTable, Table relatedModel) => CollectionFetchRelated (QueryBuilder relatedTable) relatedModel where
    collectionFetchRelated :: forall model relatedField. (
            ?modelContext :: ModelContext,
            HasField relatedField model (QueryBuilder relatedTable),
            UpdateField relatedField model (Include relatedField model) (QueryBuilder relatedTable) (FetchResult (QueryBuilder relatedTable) relatedModel),
            Fetchable (QueryBuilder relatedTable) relatedModel,
            KnownSymbol (GetTableName relatedModel),
            PG.FromRow relatedModel,
            KnownSymbol relatedField
        ) => Proxy relatedField -> [model] -> IO [Include relatedField model]
    collectionFetchRelated relatedField models = do
        let fetchRelated model = do
                let queryBuilder :: QueryBuilder relatedTable = getField @relatedField model
                result :: [relatedModel] <- fetch queryBuilder
                pure (updateField @relatedField result model)
        mapM fetchRelated models

-- Fetches a related record
--
-- Given a specific post, we can fetch the post and all its comments like this:
--
-- > let postId :: Id Post = ...
-- > 
-- > post <- fetch postId
-- >     >>= fetchRelated #comments
--
-- This Haskell code will trigger the following SQL queries to be executed:
--
-- > SELECT posts.* FROM posts WHERE id = ?  LIMIT 1
-- > SELECT comments.* FROM comments WHERE post_id = ?
--
-- In the view we can just access the comments like this:
--
-- > [hsx|
-- >     <h1>{get #title post}</h1>
-- >     <h2>Comments:</h2>
-- >     {post |> get #comments}
-- > |]
--
-- The @post |> get #comments@ returns a list of the comments belonging to the post.
-- The type of post is @Include "comments"@ Post instead of the usual @Post@. This way the state of a fetched nested resource is tracked at the type level.
--
fetchRelated :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) fieldValue (FetchResult fieldValue fetchModel),
        HasField field model fieldValue,
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel,
        Table fetchModel
    ) => Proxy field -> model -> IO (Include field model)
fetchRelated relatedField model = do
    result :: FetchResult fieldValue fetchModel <- fetch ((getField @field model) :: fieldValue)
    let model' = updateField @field result model
    pure model'
{-# INLINE fetchRelated #-}

fetchRelatedOrNothing :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) (Maybe fieldValue) (Maybe (FetchResult fieldValue fetchModel)),
        HasField field model (Maybe fieldValue),
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel,
        Table fetchModel
    ) => Proxy field -> model -> IO (Include field model)
fetchRelatedOrNothing relatedField model = do
    result :: Maybe (FetchResult fieldValue fetchModel) <- case getField @field model of
            Just fieldValue -> Just <$> fetch fieldValue
            Nothing -> pure Nothing
    let model' = updateField @field result model
    pure model'
{-# INLINE fetchRelatedOrNothing #-}

maybeFetchRelatedOrNothing :: forall model field fieldValue fetchModel. (
        ?modelContext :: ModelContext,
        UpdateField field model (Include field model) (Maybe fieldValue) (Maybe (FetchResult fieldValue fetchModel)),
        HasField field model (Maybe fieldValue),
        PG.FromRow fetchModel,
        KnownSymbol (GetTableName fetchModel),
        Fetchable fieldValue fetchModel,
        Table fetchModel
    ) => Proxy field -> Maybe model -> IO (Maybe (Include field model))
maybeFetchRelatedOrNothing relatedField = maybe (pure Nothing) (\q -> fetchRelatedOrNothing relatedField q >>= pure . Just)
{-# INLINE maybeFetchRelatedOrNothing #-}
