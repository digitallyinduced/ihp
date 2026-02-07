{-|
Module: IHP.PageHead.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.Types where

import Data.Text (Text)
import Data.IORef (IORef)
import Data.Typeable (Typeable, typeRep)
import Data.Proxy (Proxy(..))
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafePerformIO)

lookupPageHeadVault :: forall value. Typeable value => Vault.Key value -> Wai.Request -> value
lookupPageHeadVault key req =
    case Vault.lookup key (Wai.vault req) of
        Just v -> v
        Nothing -> error $ "lookupPageHeadVault: Could not find " <> show (typeRep (Proxy @value) ) <> " in request vault. Did you forget to add the PageHead middleware?"

newtype PageTitle = PageTitle Text

newtype PageDescription = PageDescription Text

newtype OGTitle = OGTitle Text

newtype OGType = OGType Text

newtype OGDescription = OGDescription Text

newtype OGUrl = OGUrl Text

newtype OGImage = OGImage Text

pageTitleVaultKey :: Vault.Key (IORef (Maybe PageTitle))
pageTitleVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pageTitleVaultKey #-}

pageDescriptionVaultKey :: Vault.Key (IORef (Maybe PageDescription))
pageDescriptionVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pageDescriptionVaultKey #-}

ogTitleVaultKey :: Vault.Key (IORef (Maybe OGTitle))
ogTitleVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE ogTitleVaultKey #-}

ogTypeVaultKey :: Vault.Key (IORef (Maybe OGType))
ogTypeVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE ogTypeVaultKey #-}

ogDescriptionVaultKey :: Vault.Key (IORef (Maybe OGDescription))
ogDescriptionVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE ogDescriptionVaultKey #-}

ogUrlVaultKey :: Vault.Key (IORef (Maybe OGUrl))
ogUrlVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE ogUrlVaultKey #-}

ogImageVaultKey :: Vault.Key (IORef (Maybe OGImage))
ogImageVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE ogImageVaultKey #-}
