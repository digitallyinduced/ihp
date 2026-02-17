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
, fetchLatest
, fetchLatestBy
)
where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Hasql.FromRow (FromRowHasql(..), HasqlDecodeColumn(..))
import IHP.QueryBuilder.HasqlCompiler (buildStatement, buildWrappedStatement)
import qualified Hasql.Decoders as Decoders
import Hasql.Implicits.Encoders (DefaultParamEncoder)
import qualified Hasql.Statement as Hasql
import IHP.Fetch.Statement (fetchByIdOneOrNothingStatement, fetchByIdListStatement)

class Fetchable fetchable model | fetchable -> model where
    type FetchResult fetchable model
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO (FetchResult fetchable model)
    fetchOneOrNothing :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO (Maybe model)
    fetchOne :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => fetchable -> IO model

-- The instance declaration had to be split up because a type variable ranging over HasQueryBuilder instances is not allowed in the declaration of the associated type. The common*-functions reduce the redundancy to the necessary minimum.
instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (QueryBuilder table) model where
    type instance FetchResult (QueryBuilder table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => QueryBuilder table -> IO [model]
    fetch = commonFetch

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => QueryBuilder table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => QueryBuilder table -> IO model
    fetchOne = commonFetchOne

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (JoinQueryBuilderWrapper r table) model where
    type instance FetchResult (JoinQueryBuilderWrapper r table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => JoinQueryBuilderWrapper r table -> IO [model]
    fetch = commonFetch

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => JoinQueryBuilderWrapper r table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => JoinQueryBuilderWrapper r table -> IO model
    fetchOne = commonFetchOne

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (NoJoinQueryBuilderWrapper table) model where
    type instance FetchResult (NoJoinQueryBuilderWrapper table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => NoJoinQueryBuilderWrapper table -> IO [model]
    fetch = commonFetch

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => NoJoinQueryBuilderWrapper table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => NoJoinQueryBuilderWrapper table -> IO model
    fetchOne = commonFetchOne

instance (model ~ GetModelByTableName table, KnownSymbol table, HasqlDecodeColumn value, KnownSymbol foreignTable, foreignModel ~ GetModelByTableName foreignTable, KnownSymbol columnName, HasField columnName foreignModel value, HasQueryBuilder (LabeledQueryBuilderWrapper foreignTable columnName value) NoJoins) => Fetchable (LabeledQueryBuilderWrapper foreignTable columnName value table) model where
    type instance FetchResult (LabeledQueryBuilderWrapper foreignTable columnName value table) model = [LabeledData value model]
    {-# INLINE fetch #-}
    fetch :: (Table model, FromRowHasql model, ?modelContext :: ModelContext) => LabeledQueryBuilderWrapper foreignTable columnName value table -> IO [LabeledData value model]
    fetch !queryBuilderProvider = do
        trackTableRead (tableName @model)
        let pool = ?modelContext.hasqlPool
        let statement = buildStatement (buildQuery queryBuilderProvider) (Decoders.rowList (hasqlRowDecoder @(LabeledData value model)))
        sqlStatementHasql pool () statement

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => LabeledQueryBuilderWrapper foreignTable columnName value table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (Table model, FromRowHasql model) => LabeledQueryBuilderWrapper foreignTable columnName value table -> IO model
    fetchOne = commonFetchOne



{-# INLINE commonFetch #-}
commonFetch :: forall model table queryBuilderProvider joinRegister. (Table model, HasQueryBuilder queryBuilderProvider joinRegister, model ~ GetModelByTableName table, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext) => queryBuilderProvider table -> IO [model]
commonFetch !queryBuilder = do
    trackTableRead (tableName @model)
    let !sqlQuery' = buildQuery queryBuilder
    let pool = ?modelContext.hasqlPool
    let statement = buildStatement sqlQuery' (Decoders.rowList (hasqlRowDecoder @model))
    sqlStatementHasql pool () statement

{-# INLINE commonFetchOneOrNothing #-}
commonFetchOneOrNothing :: forall model table queryBuilderProvider joinRegister. (?modelContext :: ModelContext) => (Table model, KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister, FromRowHasql model) => queryBuilderProvider table -> IO (Maybe model)
commonFetchOneOrNothing !queryBuilder = do
    trackTableRead (tableName @model)
    let !limitedQuery = queryBuilder |> buildQuery |> setJust #limitClause 1
    let pool = ?modelContext.hasqlPool
    let statement = buildStatement limitedQuery (Decoders.rowMaybe (hasqlRowDecoder @model))
    sqlStatementHasql pool () statement

{-# INLINE commonFetchOne #-}
commonFetchOne :: forall model table queryBuilderProvider joinRegister. (?modelContext :: ModelContext) => (Table model, KnownSymbol table, Fetchable (queryBuilderProvider table) model, HasQueryBuilder queryBuilderProvider joinRegister, FromRowHasql model) => queryBuilderProvider table -> IO model
commonFetchOne !queryBuilder = do
    maybeModel <- fetchOneOrNothing queryBuilder
    case maybeModel of
        Just model -> pure model
        Nothing -> throwIO RecordNotFoundException { queryAndParams = toSQL queryBuilder }


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
fetchCount :: forall table queryBuilderProvider joinRegister. (?modelContext :: ModelContext, KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> IO Int
fetchCount !queryBuilder = do
    let statement = buildWrappedStatement "SELECT COUNT(*) FROM (" (buildQuery queryBuilder) ") AS _count_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))
    trackTableRead (symbolToText @table)
    let pool = ?modelContext.hasqlPool
    fromIntegral <$> sqlStatementHasql pool () statement
{-# INLINE fetchCount #-}

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
fetchExists :: forall table queryBuilderProvider joinRegister. (?modelContext :: ModelContext, KnownSymbol table, HasQueryBuilder queryBuilderProvider joinRegister) => queryBuilderProvider table -> IO Bool
fetchExists !queryBuilder = do
    let statement = buildWrappedStatement "SELECT EXISTS (" (buildQuery queryBuilder) ") AS _exists_values" (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))
    trackTableRead (symbolToText @table)
    let pool = ?modelContext.hasqlPool
    sqlStatementHasql pool () statement
{-# INLINE fetchExists #-}

{-# INLINE genericFetchId #-}
genericFetchId :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder (Id' table)) => Id' table -> IO [model]
genericFetchId !id = do
    trackTableRead (tableName @model)
    sqlStatementHasql ?modelContext.hasqlPool id fetchByIdListStatement

{-# INLINE genericfetchIdOneOrNothing #-}
genericfetchIdOneOrNothing :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder (Id' table)) => Id' table -> IO (Maybe model)
genericfetchIdOneOrNothing !id = do
    trackTableRead (tableName @model)
    sqlStatementHasql ?modelContext.hasqlPool id fetchByIdOneOrNothingStatement

{-# INLINE genericFetchIdOne #-}
genericFetchIdOne :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder (Id' table)) => Id' table -> IO model
genericFetchIdOne !id = do
    trackTableRead (tableName @model)
    result <- sqlStatementHasql ?modelContext.hasqlPool id fetchByIdOneOrNothingStatement
    case result of
        Just model -> pure model
        Nothing -> throwIO RecordNotFoundException { queryAndParams = cs (Hasql.toSql (fetchByIdOneOrNothingStatement @table @model)) }

{-# INLINE genericFetchIds #-}
genericFetchIds :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIdIn ids |> fetch

{-# INLINE genericfetchIdsOneOrNothing #-}
genericfetchIdsOneOrNothing :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIdIn ids |> fetchOneOrNothing

{-# INLINE genericFetchIdsOne #-}
genericFetchIdsOne :: forall table model. (Table model, KnownSymbol table, FromRowHasql model, ?modelContext :: ModelContext, model ~ GetModelByTableName table, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey (GetTableName model)]) => [Id model] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIdIn ids |> fetchOne

{-# INLINE findBy #-}
findBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

{-# INLINE findMaybeBy #-}
findMaybeBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

--findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol name, ToField value, HasField name value model) => Proxy name -> value -> QueryBuilder model -> IO [model]
{-# INLINE findManyBy #-}
findManyBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #templateId id

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table, DefaultParamEncoder (Id' table)) => Fetchable (Id' table) model where
    type FetchResult (Id' table) model = model
    {-# INLINE fetch #-}
    fetch = genericFetchIdOne
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table, DefaultParamEncoder (Id' table)) => Fetchable (Maybe (Id' table)) model where
    type FetchResult (Maybe (Id' table)) model = [model]
    {-# INLINE fetch #-}
    fetch (Just a) = genericFetchId a
    fetch Nothing = pure []
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing Nothing = pure Nothing
    fetchOneOrNothing (Just a) = genericfetchIdOneOrNothing a
    {-# INLINE fetchOne #-}
    fetchOne (Just a) = genericFetchIdOne a
    fetchOne Nothing = error "Fetchable (Maybe Id): Failed to fetch because given id is 'Nothing', 'Just id' was expected"

instance (model ~ GetModelById (Id' table), GetModelByTableName table ~ model, GetTableName model ~ table, DefaultParamEncoder [PrimaryKey table]) => Fetchable [Id' table] model where
    type FetchResult [Id' table] model = [model]
    {-# INLINE fetch #-}
    fetch = genericFetchIds
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    {-# INLINE fetchOne #-}
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
fetchLatest :: forall table queryBuilderProvider joinRegister model.
    ( ?modelContext :: ModelContext
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    , HasField "createdAt" model UTCTime
    , Fetchable (queryBuilderProvider table) model
    , Table model
    , FromRowHasql model
    ) => queryBuilderProvider table -> IO (Maybe model)
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
fetchLatestBy :: forall table createdAt queryBuilderProvider joinRegister model.
    ( ?modelContext :: ModelContext
    , KnownSymbol createdAt
    , model ~ GetModelByTableName table
    , KnownSymbol table
    , HasQueryBuilder queryBuilderProvider joinRegister
    , HasField createdAt model UTCTime
    , Fetchable (queryBuilderProvider table) model
    , Table model
    , FromRowHasql model
    ) => Proxy createdAt -> queryBuilderProvider table -> IO (Maybe model)
fetchLatestBy field queryBuilder =
    queryBuilder
    |> orderByDesc field
    |> fetchOneOrNothing