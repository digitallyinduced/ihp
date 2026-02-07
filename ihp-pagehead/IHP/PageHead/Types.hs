{-# LANGUAGE NoFieldSelectors #-}
{-|
Module: IHP.PageHead.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.Types where

import Data.Text (Text)
import Data.Maybe (Maybe(..))
import Data.IORef (IORef)
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import GHC.Err (error)
import GHC.Show (show)
import Data.Function (($))
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafePerformIO)

newtype PageTitle = PageTitle Text

newtype PageDescription = PageDescription Text

newtype OGTitle = OGTitle Text

newtype OGType = OGType Text

newtype OGDescription = OGDescription Text

newtype OGUrl = OGUrl Text

newtype OGImage = OGImage Text

data PageHeadState = PageHeadState
    { title :: Maybe PageTitle
    , description :: Maybe PageDescription
    , ogTitle :: Maybe OGTitle
    , ogType :: Maybe OGType
    , ogDescription :: Maybe OGDescription
    , ogUrl :: Maybe OGUrl
    , ogImage :: Maybe OGImage
    }

emptyPageHeadState :: PageHeadState
emptyPageHeadState = PageHeadState Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pageHeadVaultKey :: Vault.Key (IORef PageHeadState)
pageHeadVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pageHeadVaultKey #-}

lookupPageHeadVault :: forall value. Typeable value => Vault.Key value -> Wai.Request -> value
lookupPageHeadVault key req =
    case Vault.lookup key (Wai.vault req) of
        Just v -> v
        Nothing -> error $ "lookupPageHeadVault: Could not find " <> show (typeRep (Proxy @value) ) <> " in request vault. Did you forget to add the PageHead middleware?"
