module Foundation.ValidationSupport.ValidateCanView (validateCanView) where

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
        , ValidateCanView' fieldValue fetchedModel
    ) => Proxy field -> user ->  StateT (ValidatorResultFor model) IO ()
validateCanView _ user = do
    let id = getField @field ?model
    validationResult <- liftIO $ doValidateCanView (Proxy @fetchedModel) user id
    validationState <- get
    put $ validationState & ((field @field) .~ validationResult)
    return ()


-- Let's say we have a model like:
--
--   Project { teamId :: Maybe TeamId }
--
-- Validation for the value `Project { teamId = Nothing }` should result in `Success`.
-- The usual validation logic will just do a `Project { teamId = Nothing} |> get #teamId |> fetchOneOrNothing`.
-- Simplified it's a call to `fetchOneOrNothing Nothing`, further Simplified it's `Nothing`.
-- The usual validation logic will now threat that `Nothing` like a 404 model not found error (e.g. when a invalid project id is given).
--
-- Therefore we have to handle this special of `Maybe TeamId` with the following type class.
class ValidateCanView' id model where
    doValidateCanView :: (?modelContext :: ModelContext, CanView user model, Fetchable id model, KnownSymbol (GetTableName model), PG.FromRow model) => Proxy model -> user -> id -> IO ValidatorResult

-- Maybe someId
instance {-# OVERLAPS #-} (ValidateCanView' id' model, Fetchable id' model) => ValidateCanView' (Maybe id') model where
    -- doValidateCanView :: (?modelContext :: ModelContext, CanView user model, Fetchable id model, KnownSymbol (GetTableName model), PG.FromRow model) => Proxy model -> user -> (Maybe id) -> IO ValidatorResult
    doValidateCanView model user id = maybe (return Success) (doValidateCanView model user) id

-- Catch all
instance {-# OVERLAPPABLE #-} ValidateCanView' any model where
    doValidateCanView :: (?modelContext :: ModelContext, CanView user model, Fetchable id model, KnownSymbol (GetTableName model), PG.FromRow model) => Proxy model -> user -> id -> IO ValidatorResult
    doValidateCanView model user id = do
        fetchedModel <- liftIO (fetchOneOrNothing id)
        canView' <- maybe (return False) (\fetchedModel -> canView fetchedModel user) fetchedModel
        return $ if canView'
            then Success
            else Failure "Please pick something"
