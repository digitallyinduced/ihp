module Foundation.ValidationSupport.ValidateCanView where

import           ClassyPrelude
import           Control.Lens                         hiding ((|>))
import           Data.Generics.Product
import           Data.Generics.Product.Types
import           Data.Proxy
import qualified Data.Text                            as Text
import qualified Data.UUID
import qualified Database.PostgreSQL.Simple           as PG
import           Foundation.AuthSupport.Authorization
import           Foundation.ModelSupport
import           Foundation.NameSupport               (humanize)
import           Foundation.QueryBuilder              (Fetchable, fetchOneOrNothing)
import           Foundation.ValidationSupport.Types
import           GHC.Generics
import           GHC.Records                          hiding (HasField, getField)
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import Control.Monad.State

validateCanView :: forall field user model validationState fieldValue validationStateValue fetchedModel. (
        ?model :: model
        , ?modelContext :: ModelContext
        , PG.FromRow fetchedModel
        , KnownSymbol (GetTableName fetchedModel)
        , KnownSymbol field
        , HasField' field model fieldValue
        , HasField field (ValidatorResultFor model) (ValidatorResultFor model) ValidatorResult ValidatorResult
        , Fetchable fieldValue fetchedModel
        , CanView user fetchedModel
    ) => Proxy field -> user ->  StateT (ValidatorResultFor model) IO ()
validateCanView _ user = do
    let id = getField @field ?model
    fetchedModel <- liftIO (fetchOneOrNothing id)
    canView' <- maybe (return False) (\fetchedModel -> liftIO $ canView fetchedModel user) fetchedModel
    let validationResult = if canView'
        then Success
        else Failure "Cannot access that model"
    validationState <- get
    put $ validationState & ((field @field) .~ validationResult)
    return ()
