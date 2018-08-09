{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, AllowAmbiguousTypes #-}

module Foundation.FetchRelated (fetchRelated, collectionFetchRelated) where

import Control.Lens hiding ((|>))
import Data.Generics.Product
import GHC.Generics
import Foundation.HaskellSupport
import ClassyPrelude hiding (UTCTime, find)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String.Conversions (cs)
import Data.Time.Clock (UTCTime)
import Unsafe.Coerce
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
import GHC.TypeLits
import GHC.Types
import Data.Proxy
import Foundation.ModelSupport (ModelFieldValue, GetTableName, ModelContext, GetModelById, NewTypeWrappedUUID)
import qualified Foundation.ModelSupport
import Foundation.NameSupport (fieldNameToColumnName)
import Foundation.QueryBuilder

collectionFetchRelated :: forall model relatedField relatedFieldValue relatedModel. (
        ?modelContext :: ModelContext,
        HasField' relatedField model relatedFieldValue,
        HasField' "id" relatedModel relatedFieldValue,
        HasField relatedField model (Foundation.ModelSupport.Include relatedField model) relatedFieldValue relatedModel,
        Fetchable relatedFieldValue relatedModel,
        KnownSymbol (GetTableName relatedModel),
        PG.FromRow relatedModel,
        relatedFieldValue ~ ModelFieldValue model relatedField,
        Eq relatedFieldValue,
        ToField relatedFieldValue,
        KnownSymbol relatedField,
        ModelFieldValue relatedModel "id" ~ relatedFieldValue
    ) => Proxy relatedField -> [model] -> IO [Foundation.ModelSupport.Include relatedField model]
collectionFetchRelated relatedField model = do
    relatedModels :: [relatedModel] <- query @relatedModel |> filterWhereIn (#id, map (getField @relatedField) model) |> fetch
    let
        assignRelated :: model -> Foundation.ModelSupport.Include relatedField model
        assignRelated model =
            let
                relatedModel :: relatedModel
                (Just relatedModel) = ClassyPrelude.find (\r -> (getField @"id" r :: relatedFieldValue) == (getField @relatedField model :: relatedFieldValue)) relatedModels
            in
                model & field @relatedField .~ relatedModel

    let
        result :: [Foundation.ModelSupport.Include relatedField model]
        result = map assignRelated model
    return result

fetchRelated :: forall model relatedField relatedFieldValue relatedModel. (
        ?modelContext :: ModelContext,
        KnownSymbol relatedField,
        HasField' relatedField model relatedFieldValue,
        HasField relatedField model (Foundation.ModelSupport.Include relatedField model) relatedFieldValue relatedModel,
        Fetchable relatedFieldValue relatedModel,
        KnownSymbol (GetTableName relatedModel),
        PG.FromRow relatedModel,
        relatedFieldValue ~ ModelFieldValue model relatedField,
        ToField relatedFieldValue,
        ModelFieldValue relatedModel "id" ~ relatedFieldValue
    ) => Proxy relatedField -> model -> IO (Foundation.ModelSupport.Include relatedField model)
fetchRelated relatedField model = do
    relatedModel :: relatedModel <- fetchOne ((getField @relatedField model) :: relatedFieldValue)
    let model' = model & field @relatedField .~ relatedModel
    return model'
