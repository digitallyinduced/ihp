{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, FunctionalDependencies, FlexibleContexts, AllowAmbiguousTypes #-}

module TurboHaskell.FetchRelated (fetchRelated, collectionFetchRelated) where

import TurboHaskell.HaskellSupport
import ClassyPrelude hiding (UTCTime, find)
import qualified ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections
import Database.PostgreSQL.Simple.Types (Query (Query), In (In))
import Database.PostgreSQL.Simple.FromField hiding (Field, name)
import Database.PostgreSQL.Simple.ToField
import Data.Default
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock (UTCTime)
import Unsafe.Coerce
import Data.UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
import GHC.TypeLits
import GHC.Types
import GHC.Records
import Data.Proxy
import TurboHaskell.ModelSupport (GetTableName, ModelContext, GetModelById)
import qualified TurboHaskell.ModelSupport
import TurboHaskell.ModelSupport (Include)
import TurboHaskell.NameSupport (fieldNameToColumnName)
import TurboHaskell.QueryBuilder

collectionFetchRelated :: forall model relatedField relatedFieldValue relatedModel. (
        ?modelContext :: ModelContext,
        HasField relatedField model relatedFieldValue,
        HasField "id" relatedModel relatedFieldValue,
        UpdateField relatedField model (TurboHaskell.ModelSupport.Include relatedField model) relatedFieldValue relatedModel,
        Fetchable relatedFieldValue relatedModel,
        KnownSymbol (GetTableName relatedModel),
        PG.FromRow relatedModel,
        HasField relatedField model relatedFieldValue,
        Eq relatedFieldValue,
        ToField relatedFieldValue,
        KnownSymbol relatedField,
        HasField "id" relatedModel relatedFieldValue
    ) => Proxy relatedField -> [model] -> IO [TurboHaskell.ModelSupport.Include relatedField model]
collectionFetchRelated relatedField model = do
    relatedModels :: [relatedModel] <- query @relatedModel |> filterWhereIn (#id, map (getField @relatedField) model) |> fetch
    let
        assignRelated :: model -> TurboHaskell.ModelSupport.Include relatedField model
        assignRelated model =
            let
                relatedModel :: relatedModel
                (Just relatedModel) = ClassyPrelude.find (\r -> (getField @"id" r :: relatedFieldValue) == (getField @relatedField model :: relatedFieldValue)) relatedModels
            in
                updateField @relatedField relatedModel model

    let
        result :: [TurboHaskell.ModelSupport.Include relatedField model]
        result = map assignRelated model
    return result

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
    return model'
