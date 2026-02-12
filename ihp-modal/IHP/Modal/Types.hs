{-|
Module: IHP.Modal.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.Types
( Modal (..)
, ModalContainer (..)
, modalContainerVaultKey
, lookupModalVault
) where

import Prelude
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))
import Text.Blaze.Html5 (Html)
import qualified Data.Vault.Lazy as Vault
import Network.Wai
import System.IO.Unsafe (unsafePerformIO)

data Modal = Modal
    { modalContent :: Html
    , modalFooter :: Maybe Html
    , modalCloseUrl :: Text
    , modalTitle :: Text
    }

-- | Stores the current modal inside the request vault
newtype ModalContainer = ModalContainer Html

modalContainerVaultKey :: Vault.Key (IORef (Maybe ModalContainer))
modalContainerVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE modalContainerVaultKey #-}

lookupModalVault :: forall value. Typeable value => Vault.Key value -> Request -> value
lookupModalVault key req =
    case Vault.lookup key req.vault of
        Just v -> v
        Nothing -> error $ "lookupModalVault: Could not find " <> show (typeRep (Proxy @value) ) <> " in request vault. Did you forget to add the Modal middleware?"
