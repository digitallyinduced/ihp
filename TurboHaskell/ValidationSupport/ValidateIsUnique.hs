module TurboHaskell.ValidationSupport.ValidateIsUnique (validateIsUnique) where

import           ClassyPrelude
import           Data.Proxy
import qualified Data.Text                            as Text
import qualified Data.UUID
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import           TurboHaskell.AuthSupport.Authorization
import           TurboHaskell.ModelSupport
import           TurboHaskell.NameSupport               (humanize)
import           TurboHaskell.ValidationSupport.Types
import           GHC.Generics
import           GHC.Records
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State
import TurboHaskell.HaskellSupport hiding (get)
import TurboHaskell.QueryBuilder

{-# INLINE validateIsUnique #-}
validateIsUnique :: forall field model savedModel validationState fieldValue validationStateValue fetchedModel. (
        savedModel ~ NormalizeModel model
        , ?modelContext :: ModelContext
        , PG.FromRow savedModel
        , KnownSymbol field
        , HasField field model fieldValue
        , HasField field savedModel fieldValue
        , KnownSymbol (GetTableName savedModel)
        , PG.ToField fieldValue
        , EqOrIsOperator fieldValue
    ) => Proxy field -> model -> StateT [(Text, Text)] IO model
validateIsUnique fieldProxy model = do
    let value = getField @field model
    result <- query @savedModel
        |> filterWhere (fieldProxy, value)
        |> fetchOneOrNothing
        |> liftIO
    case result of
        Just _ -> do
            attachFailure (Proxy @field) "This is already in use"
            return model
        Nothing -> return model
