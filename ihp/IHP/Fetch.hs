{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.Fetch
Description: fetch, fetchOne, fetchOneOrNothing and friends
Copyright: (c) digitally induced GmbH, 2020

This modules builds on top of 'IHP.QueryBuilder' and provides functions to fetch a query builder.

For more complex sql queries, use 'IHP.ModelSupport.sqlQuery'.
-}
module IHP.Fetch
( findManyBy
, findMaybeBy
, findBy
, genericFetchId
, genericfetchIdOneOrNothing
, genericFetchIdOne
, Fetchable (..)
, genericFetchIds
, genericfetchIdsOneOrNothing
, genericFetchIdsOne
, fetchCount
, fetchExists
, fetchVector
, fetchLatest
, fetchLatestBy
)
where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Hasql.FromRow (FromRowHasql(..))
import Hasql.Implicits.Encoders (DefaultParamEncoder)
import IHP.Fetch.Statement (buildQueryListStatement, buildQueryVectorStatement, buildQueryMaybeStatement, buildCountStatement, buildExistsStatement)

class Fetchable fetchable model | fetchable -> model where
    type FetchResult fetchable model
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO (FetchResult fetchable model)
    fetchOneOrNothing :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO (Maybe model)
    fetchOne :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO model

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (QueryBuilder table) model where
    type instance FetchResult (QueryBuilder table) model = [model]
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => QueryBuilder table -> IO [model]
    fetch !queryBuilder = do
        trackTableRead (tableName @model)
        let pool = ?modelContext.hasqlPool
        sqlStatementHasql pool () (buildQueryListStatement queryBuilder)

    fetchOneOrNothing :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => QueryBuilder table -> IO (Maybe model)
    fetchOneOrNothing !queryBuilder = do
        trackTableRead (tableName @model)
        let pool = ?modelContext.hasqlPool
        sqlStatementHasql pool () (buildQueryMaybeStatement queryBuilder)

    fetchOne :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => QueryBuilder table -> IO model
    fetchOne !queryBuilder = do
        maybeModel <- fetchOneOrNothing queryBuilder
        case maybeModel of
            Just model -> pure model
            Nothing -> throwIO RecordNotFoundException { queryAndParams = toSQL queryBuilder }


-- | Like 'fetch', but returns a 'Vector' instead of a list for better performance
-- with large result sets.
--
-- __Example:__ Fetching all users as a Vector
--
-- > allUsers <- query @User |> fetchVector
--
-- __Example:__ Fetching with filters
--
-- > activeUsers <- query @User
-- >     |> filterWhere (#active, True)
-- >     |> fetchVector
fetchVector :: forall model table. (Table model, model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext) => QueryBuilder table -> IO (Vector model)
fetchVector !queryBuilder = do
    trackTableRead (tableName @model)
    let pool = ?modelContext.hasqlPool
    sqlStatementHasql pool () (buildQueryVectorStatement queryBuilder)

-- | Returns the count of records selected by the query builder.
--
-- __Example:__ Counting all users.
--
-- > allUsersCount <- query @User |> fetchCount -- SELECT COUNT(*) FROM users
--
--
-- __Example:__ Counting all active projects
--
-- >     activeProjectsCount <- query @Project
-- >         |> filterWhere (#isActive, True)
-- >         |> fetchCount
-- >     -- SELECT COUNT(*) FROM projects WHERE is_active = true
fetchCount :: forall table. (?modelContext :: ModelContext, KnownSymbol table) => QueryBuilder table -> IO Int
fetchCount !queryBuilder = do
    trackTableRead (symbolToText @table)
    let pool = ?modelContext.hasqlPool
    fromIntegral <$> sqlStatementHasql pool () (buildCountStatement queryBuilder)

-- | Checks whether the query has any results.
--
-- Returns @True@ when there is at least one row matching the conditions of the query. Returns @False@ otherwise.
--
-- __Example:__ Checking whether there are unread messages
--
-- >     hasUnreadMessages <- query @Message
-- >         |> filterWhere (#isUnread, True)
-- >         |> fetchExists
-- >     -- SELECT EXISTS (SELECT * FROM messages WHERE is_unread = true)
fetchExists :: forall table. (?modelContext :: ModelContext, KnownSymbol table) => QueryBuilder table -> IO Bool
fetchExists !queryBuilder = do
    trackTableRead (symbolToText @table)
    let pool = ?modelContext.hasqlPool
    sqlStatementHasql pool () (buildExistsStatement queryBuilder)

genericFetchId :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, FilterPrimaryKey table) => Id' table -> IO [model]
genericFetchId !id = query @model |> filterWhereId id |> fetch

genericfetchIdOneOrNothing :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, FilterPrimaryKey table) => Id' table -> IO (Maybe model)
genericfetchIdOneOrNothing !id = query @model |> filterWhereId id |> fetchOneOrNothing

genericFetchIdOne :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, FilterPrimaryKey table) => Id' table -> IO model
genericFetchIdOne !id = query @model |> filterWhereId id |> fetchOne

genericFetchIds :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIdIn ids |> fetch

genericfetchIdsOneOrNothing :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIdIn ids |> fetchOneOrNothing

genericFetchIdsOne :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIdIn ids |> fetchOne

findBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

findMaybeBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

--findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol name, ToField value, HasField name value model) => Proxy name -> value -> QueryBuilder model -> IO [model]
findManyBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #templateId id

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Id' table) model where
    type FetchResult (Id' table) model = model
    fetch = genericFetchIdOne
    fetchOneOrNothing = genericfetchIdOneOrNothing
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Maybe (Id' table)) model where
    type FetchResult (Maybe (Id' table)) model = [model]
    fetch (Just a) = genericFetchId a
    fetch Nothing = pure []
    fetchOneOrNothing Nothing = pure Nothing
    fetchOneOrNothing (Just a) = genericfetchIdOneOrNothing a
    fetchOne (Just a) = genericFetchIdOne a
    fetchOne Nothing = error "Fetchable (Maybe Id): Failed to fetch because given id is 'Nothing', 'Just id' was expected"

instance (model ~ GetModelById (Id' table), GetModelByTableName table ~ model, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey table]) => Fetchable [Id' table] model where
    type FetchResult [Id' table] model = [model]
    fetch = genericFetchIds
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    fetchOne = genericFetchIdsOne

-- | Returns the latest record or Nothing
--
-- __Example:__
--
-- > latestUser <-
-- >     query @User
-- >         |> fetchLatest
-- >
--
-- 'fetchLatest' is mainly a shortcut for code like this:
--
-- > latestUser <-
-- >     query @User
-- >         |> orderByDesc #createdAt
-- >         |> fetchOneOrNothing
--
fetchLatest :: forall table model.
    ( ?modelContext :: ModelContext
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , HasField "createdAt" model UTCTime
    , Table model
    , FromRowHasql model
    ) => QueryBuilder table -> IO (Maybe model)
fetchLatest queryBuilder = queryBuilder |> fetchLatestBy #createdAt

-- | Provided a field name, it returns the latest record or Nothing
--
-- See 'fetchLatest' if you're looking for the latest record by the createdAt timestamp.
--
-- __Example:__
--
-- > latestTrialUser <-
-- >     query @User
-- >         |> fetchLatestBy #trialStartedAt
-- >
--
-- 'fetchLatestBy' is mainly a shortcut for code like this:
--
-- > latestUser <-
-- >     query @User
-- >         |> orderByDesc #trialStartedAt
-- >         |> fetchOneOrNothing
--
fetchLatestBy :: forall table createdAt model.
    ( ?modelContext :: ModelContext
    , KnownSymbol createdAt
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , HasField createdAt model UTCTime
    , Table model
    , FromRowHasql model
    ) => Proxy createdAt -> QueryBuilder table -> IO (Maybe model)
fetchLatestBy field queryBuilder =
    queryBuilder
    |> orderByDesc field
    |> fetchOneOrNothing
