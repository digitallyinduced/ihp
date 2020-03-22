module TurboHaskell.ValidationSupport.ValidateCanView (validateCanView) where

import           ClassyPrelude
import           Data.Proxy
import qualified Database.PostgreSQL.Simple           as PG
import           TurboHaskell.AuthSupport.Authorization
import           TurboHaskell.ModelSupport
import           TurboHaskell.QueryBuilder              (Fetchable, fetchOneOrNothing)
import           TurboHaskell.ValidationSupport.Types
import           GHC.Records
import           GHC.TypeLits                         (KnownSymbol, Symbol)
import TurboHaskell.HaskellSupport

validateCanView :: forall field user model validationState fieldValue validationStateValue fetchedModel. (
        ?model :: model
        , ?modelContext :: ModelContext
        , PG.FromRow fetchedModel
        , KnownSymbol (GetTableName fetchedModel)
        , KnownSymbol field
        , HasField field model fieldValue
        , Fetchable fieldValue fetchedModel
        , CanView user fetchedModel
        , ValidateCanView' fieldValue fetchedModel
        , HasField "meta" user MetaBag
        , SetField "meta" user MetaBag
    ) => Proxy field -> user -> IO user
validateCanView field user = do
    let id = getField @field ?model
    validationResult <- doValidateCanView (Proxy @fetchedModel) user id
    pure (attachValidatorResult field validationResult user)


-- | Let's say we have a model like:
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
    doValidateCanView model user id = maybe (pure Success) (doValidateCanView model user) id

-- Catch all
instance {-# OVERLAPPABLE #-} ValidateCanView' any model where
    doValidateCanView :: (?modelContext :: ModelContext, CanView user model, Fetchable id model, KnownSymbol (GetTableName model), PG.FromRow model) => Proxy model -> user -> id -> IO ValidatorResult
    doValidateCanView model user id = do
        fetchedModel <- liftIO (fetchOneOrNothing id)
        canView' <- maybe (pure False) (\fetchedModel -> canView fetchedModel user) fetchedModel
        pure $ if canView'
            then Success
            else Failure "Please pick something"
