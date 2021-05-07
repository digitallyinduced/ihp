{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
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
, In (In)
, genericFetchId
, genericfetchIdOneOrNothing
, genericFetchIdOne
, Fetchable (..)
, genericFetchIds
, genericfetchIdsOneOrNothing
, genericFetchIdsOne
, fetchCount
, fetchExists
)
where

import IHP.Prelude
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.OverloadedLabels
import IHP.ModelSupport
import qualified Data.ByteString.Builder as Builder
import IHP.HtmlSupport.ToHtml
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text.Encoding as Text
import IHP.QueryBuilder

class Fetchable fetchable model | fetchable -> model where
    type FetchResult fetchable model
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO (FetchResult fetchable model)
    fetchOneOrNothing :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO (Maybe model)
    fetchOne :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => fetchable -> IO model

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (QueryBuilder table) model where
    type instance FetchResult (QueryBuilder table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => QueryBuilder table -> IO [model]
    fetch = commonFetch 

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => QueryBuilder table -> IO model
    fetchOne = commonFetchOne

instance (model ~ GetModelByTableName table, KnownSymbol table) => Fetchable (JoinQueryBuilderWrapper r table) model where
    type instance FetchResult (JoinQueryBuilderWrapper r table) model = [model]
    {-# INLINE fetch #-}
    fetch :: (KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => JoinQueryBuilderWrapper r table -> IO [model]
    fetch = commonFetch 

    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => JoinQueryBuilderWrapper r table -> IO (Maybe model)
    fetchOneOrNothing = commonFetchOneOrNothing

    {-# INLINE fetchOne #-}
    fetchOne :: (?modelContext :: ModelContext) => (PG.FromRow model, KnownSymbol (GetTableName model)) => JoinQueryBuilderWrapper r table -> IO model
    fetchOne = commonFetchOne


{-# INLINE commonFetch #-}
commonFetch :: forall model table q r.(HasQueryBuilder q r, model ~ GetModelByTableName table, KnownSymbol table, KnownSymbol (GetTableName model), PG.FromRow model, ?modelContext :: ModelContext) => q table -> IO [model]
commonFetch !queryBuilder = do
    let !(theQuery, theParameters) = queryBuilder
            |> toSQL
    trackTableRead (tableNameByteString @model)
    sqlQuery (Query $ cs theQuery) theParameters

{-# INLINE commonFetchOneOrNothing #-}
commonFetchOneOrNothing :: forall model table q r.(?modelContext :: ModelContext) => (KnownSymbol table, HasQueryBuilder q r, PG.FromRow model, KnownSymbol (GetTableName model)) => q table -> IO (Maybe model)
commonFetchOneOrNothing !queryBuilder = do
    let !(theQuery, theParameters) = queryBuilder
            |> buildQuery
            |> setJust #limitClause "LIMIT 1"
            |> toSQL'
    trackTableRead (tableNameByteString @model)
    results <- sqlQuery (Query $ cs theQuery) theParameters
    pure $ listToMaybe results

{-# INLINE commonFetchOne #-}
commonFetchOne :: (?modelContext :: ModelContext) => (KnownSymbol table, Fetchable (q table) model, HasQueryBuilder q r, PG.FromRow model, KnownSymbol (GetTableName model)) => q table -> IO model
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
fetchCount :: forall table q r. (?modelContext :: ModelContext, KnownSymbol table, HasQueryBuilder q r) => q table -> IO Int
fetchCount !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT COUNT(*) FROM (" <> theQuery' <> ") AS _count_values"
    trackTableRead (symbolToByteString @table)
    [PG.Only count] <- sqlQuery (Query $! cs theQuery) theParameters
    pure count
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
fetchExists :: forall table q r. (?modelContext :: ModelContext, KnownSymbol table, HasQueryBuilder q r) => q table -> IO Bool
fetchExists !queryBuilder = do
    let !(theQuery', theParameters) = toSQL' (buildQuery queryBuilder)
    let theQuery = "SELECT EXISTS (" <> theQuery' <> ") AS _exists_values"
    trackTableRead (symbolToByteString @table)
    [PG.Only exists] <- sqlQuery (Query $! cs theQuery) theParameters
    pure exists
{-# INLINE fetchExists #-}

{-# INLINE genericFetchId #-}
genericFetchId :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO [model]
genericFetchId !id = query @model |> filterWhereId id |> fetch

{-# INLINE genericfetchIdOneOrNothing #-}
genericfetchIdOneOrNothing :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO (Maybe model)
genericfetchIdOneOrNothing !id = query @model |> filterWhereId id |> fetchOneOrNothing

{-# INLINE genericFetchIdOne #-}
genericFetchIdOne :: forall table model. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, FilterPrimaryKey table, model ~ GetModelByTableName table, GetTableName model ~ table) => Id' table -> IO model
genericFetchIdOne !id = query @model |> filterWhereId id |> fetchOne

{-# INLINE genericFetchIds #-}
genericFetchIds :: forall table model value. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO [model]
genericFetchIds !ids = query @model |> filterWhereIn (#id, ids) |> fetch

{-# INLINE genericfetchIdsOneOrNothing #-}
genericfetchIdsOneOrNothing :: forall model value table. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO (Maybe model)
genericfetchIdsOneOrNothing !ids = query @model |> filterWhereIn (#id, ids) |> fetchOneOrNothing

{-# INLINE genericFetchIdsOne #-}
genericFetchIdsOne :: forall model value table. (KnownSymbol table, PG.FromRow model, ?modelContext :: ModelContext, ToField value, EqOrIsOperator value, HasField "id" model value, model ~ GetModelByTableName table, GetTableName model ~ table) => [value] -> IO model
genericFetchIdsOne !ids = query @model |> filterWhereIn (#id, ids) |> fetchOne

{-# INLINE findBy #-}
findBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOne

{-# INLINE findMaybeBy #-}
findMaybeBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetchOneOrNothing

--findManyBy :: (?modelContext :: ModelContext, PG.FromRow model, KnownSymbol (GetTableName model), KnownSymbol name, ToField value, HasField name value model) => Proxy name -> value -> QueryBuilder model -> IO [model]
{-# INLINE findManyBy #-}
findManyBy !field !value !queryBuilder = queryBuilder |> filterWhere (field, value) |> fetch
-- Step.findOneByWorkflowId id    ==    queryBuilder |> findBy #templateId id

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Id' table) model where
    type FetchResult (Id' table) model = model
    {-# INLINE fetch #-}
    fetch = genericFetchIdOne
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdOne

instance (model ~ GetModelById (Id' table), GetTableName model ~ table, FilterPrimaryKey table) => Fetchable (Maybe (Id' table)) model where
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

instance (model ~ GetModelById (Id' table), value ~ Id' table, HasField "id" model value, ToField (PrimaryKey table), GetModelByTableName (GetTableName model) ~ model) => Fetchable [Id' table] model where
    type FetchResult [Id' table] model = [model]
    {-# INLINE fetch #-}
    fetch = genericFetchIds
    {-# INLINE fetchOneOrNothing #-}
    fetchOneOrNothing = genericfetchIdsOneOrNothing
    {-# INLINE fetchOne #-}
    fetchOne = genericFetchIdsOne
