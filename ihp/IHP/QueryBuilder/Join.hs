{-# LANGUAGE BangPatterns, TypeFamilies, DataKinds, PolyKinds, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, InstanceSigs, AllowAmbiguousTypes, DeriveAnyClass #-}
{-|
Module: IHP.QueryBuilder.Join
Description: JOIN operations for QueryBuilder
Copyright: (c) digitally induced GmbH, 2020

This module provides functions for joining tables in queries.
-}
module IHP.QueryBuilder.Join
( innerJoin
, innerJoinThirdTable
, labelResults
) where

import IHP.Prelude
import IHP.ModelSupport
import IHP.QueryBuilder.Types
import IHP.QueryBuilder.Compiler (qualifiedColumnName)

-- | Joins a table to an existing QueryBuilder (or something holding a QueryBuilder) on the specified columns. Example:
-- >    query @Posts
-- > |> innerJoin @Users (#author, #id)
-- > -- SELECT users.* FROM users INNER JOIN posts ON users.id = posts.author ...
innerJoin :: forall model' table' name' value' model table name value queryBuilderProvider joinRegister.
                            (
                                KnownSymbol name,
                                KnownSymbol table,
                                HasField name model value,
                                KnownSymbol name',
                                KnownSymbol table',
                                HasQueryBuilder queryBuilderProvider joinRegister,
                                ModelList joinRegister,
                                HasField name' model' value',
                                value ~ value',
                                model ~ GetModelByTableName table,
                                table' ~ GetTableName model'
                            ) => (Proxy name, Proxy name') -> queryBuilderProvider table -> JoinQueryBuilderWrapper (ConsModelList model' joinRegister) table
innerJoin (name, name') queryBuilderProvider = injectQueryBuilder $ JoinQueryBuilder (getQueryBuilder queryBuilderProvider) $ Join joinTableName leftJoinColumn rightJoinColumn
    where
        baseTableName = symbolToText @table
        joinTableName = symbolToText @table'
        leftJoinColumn = qualifiedColumnName baseTableName (symbolToText @name)
        rightJoinColumn = fieldNameToColumnName (symbolToText @name')
{-# INLINE innerJoin #-}

-- | Index the values from a table with values of a field from a table joined by 'innerJoin' or 'innerJoinThirdTable'. Useful to get, e.g., the tags to a set of posts in such a way that the assignment of tags to posts is preserved.
--
--
-- __Example:__ Fetch a list of all comments, each paired with the id of the post it belongs to.
--
-- > labeledTags <-
-- >  query @Tag
-- >     |> innerJoin @Tagging (#id, #tagId)
-- >     |> innerJoinThirdTable @Post @Tagging (#id, #postId)
-- >     |> labelResults @Post #id
-- >     |> fetch
-- > -- SELECT posts.id, tags.* FROM comments INNER JOIN taggings ON tags.id = taggings.tagId INNER JOIN posts ON posts.id = taggings.postId
--
-- labeledTags is then a list of type ['LabeledData' (Id' "posts") Tag] such that "LabeledData postId tag" is contained in that list if "tag" is a tag of the post with id postId.
--
labelResults :: forall foreignModel baseModel foreignTable baseTable name value queryBuilderProvider joinRegister.
                (
                    KnownSymbol foreignTable,
                    KnownSymbol baseTable,
                    foreignTable ~ GetTableName foreignModel,
                    baseModel ~ GetModelByTableName baseTable,
                    HasField name foreignModel value,
                    HasQueryBuilder queryBuilderProvider joinRegister,
                    KnownSymbol name,
                    IsJoined foreignModel joinRegister
                ) => Proxy name -> queryBuilderProvider baseTable -> LabeledQueryBuilderWrapper foreignTable name value baseTable
labelResults name queryBuilderProvider = LabeledQueryBuilderWrapper $ getQueryBuilder queryBuilderProvider

-- | Joins a table on a column held by a previously joined table. Example:
-- > query @Posts
-- > |> innerJoin @Users (#author, #id)
-- > |> innerJoinThirdTable @City @Users (#id, #homeTown)
-- > -- SELECT posts.* FROM posts INNER JOIN users ON posts.author = users.id INNER JOIN cities ON user.home_town = cities.id
--
innerJoinThirdTable :: forall model model' name name' value value' table table' baseTable baseModel queryBuilderProvider joinRegister.
                        (
                            KnownSymbol name,
                            KnownSymbol table,
                            HasField name model value,
                            KnownSymbol name',
                            KnownSymbol table',
                            HasQueryBuilder queryBuilderProvider joinRegister,
                            ModelList joinRegister,
                            HasField name' model' value',
                            value ~ value',
                            table ~ GetTableName model,
                            table' ~ GetTableName model',
                            baseModel ~ GetModelByTableName baseTable
                        ) => (Proxy name, Proxy name') -> queryBuilderProvider baseTable -> JoinQueryBuilderWrapper (ConsModelList model joinRegister) baseTable
innerJoinThirdTable (name, name') queryBuilderProvider = injectQueryBuilder $ JoinQueryBuilder (getQueryBuilder queryBuilderProvider) $ Join joinTableName leftJoinColumn rightJoinColumn
     where
        baseTableName = symbolToText @table'
        joinTableName = symbolToText @table
        leftJoinColumn = qualifiedColumnName baseTableName (symbolToText @name')
        rightJoinColumn = fieldNameToColumnName (symbolToText @name)
{-# INLINE innerJoinThirdTable #-}
