{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.ActionType
Copyright: (c) digitally induced GmbH, 2025

Provides ActionType for tracking the current controller action type
in the WAI request vault.
-}
module IHP.ActionType
( ActionType(..)
, actionTypeVaultKey
, requestActionType
, setActionType
, isActiveController
) where

import Prelude
import GHC.Records (HasField(..))
import Data.Typeable (Typeable)
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Typeable as Typeable
import Data.Proxy

-- | Used to track the current action type
newtype ActionType = ActionType Typeable.TypeRep

actionTypeVaultKey :: Vault.Key ActionType
actionTypeVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE actionTypeVaultKey #-}

requestActionType :: Request -> ActionType
requestActionType req =
    case Vault.lookup actionTypeVaultKey req.vault of
        Just actionType -> actionType
        Nothing -> error "requestActionType: ActionType not found in request vault"

-- | Insert the ActionType into the request vault
{-# INLINE setActionType #-}
setActionType :: Typeable controller => controller -> Request -> Request
setActionType controller req = req { vault = Vault.insert actionTypeVaultKey (ActionType (Typeable.typeOf controller)) req.vault }

instance HasField "actionType" Request ActionType where
    getField = requestActionType

-- | Returns @True@ when the given type matches the type of the currently executed controller action
--
-- __Example:__ The browser has requested @\/Posts@ and the @Posts@ action of the @PostsController@ is called.
--
-- >>> isActiveController @PostsController
-- True
--
-- Returns @True@ because the current action is part of the @PostsController@
isActiveController :: forall controller. (?request :: Request, Typeable controller) => Bool
isActiveController =
    let
        (ActionType actionType) = ?request.actionType
    in
        (Typeable.typeRep (Proxy @controller)) == actionType
