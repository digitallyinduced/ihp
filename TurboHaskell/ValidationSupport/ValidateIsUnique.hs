module TurboHaskell.ValidationSupport.ValidateIsUnique (validateIsUnique) where

import           ClassyPrelude
import           Control.Lens                         hiding ((|>))
import           Data.Generics.Product
import           Data.Generics.Product.Types
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
import           GHC.Records                          hiding (HasField, getField)
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State
import TurboHaskell.HaskellSupport hiding (get)
import TurboHaskell.QueryBuilder

validateIsUnique :: forall field model savedModel validationState fieldValue validationStateValue fetchedModel. (
        ?model :: model
        , savedModel ~ GetModelById (ModelFieldValue model "id")
        , ?modelContext :: ModelContext
        , PG.FromRow savedModel
        , KnownSymbol field
        , HasField' field model fieldValue
        , HasField field (ValidatorResultFor model) (ValidatorResultFor model) ValidatorResult ValidatorResult
        , HasField' field savedModel fieldValue
        , KnownSymbol (GetTableName savedModel)
        , PG.ToField fieldValue
        , EqOrIsOperator fieldValue
    ) => Proxy field ->  StateT (ValidatorResultFor model) IO ()
validateIsUnique fieldProxy = do
    let value = getField @field ?model
    result <- liftIO $ query @savedModel |> filterWhere (fieldProxy, value) |> fetchOneOrNothing
    case result of
        Just _ -> do
            let validatorResult :: ValidatorResult = Failure "This is already in use"
            validationState <- get
            put $ validationState & ((field @field) .~ validatorResult)
        Nothing -> return ()
