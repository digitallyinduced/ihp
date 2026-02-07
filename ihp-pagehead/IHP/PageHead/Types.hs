{-# LANGUAGE NoFieldSelectors #-}
{-|
Module: IHP.PageHead.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.Types where

import Data.Text (Text)
import Data.Maybe (Maybe(..))
import Data.IORef (IORef)
import qualified Data.Vault.Lazy as Vault
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
