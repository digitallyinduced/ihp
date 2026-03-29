{-|
Module: IHP.PageHead.ControllerFunctions
Description: Manage the @<title>@ and @<meta>@ tags of your HTML pages
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.ControllerFunctions
( setTitle
, setDescription
, setOGTitle
, setOGType
, setOGDescription
, setOGUrl
, setOGImage
) where

import Prelude
import Data.IORef (modifyIORef')
import Data.Text (Text)
import Network.Wai (Request)
import IHP.PageHead.Types

-- | Sets the page title. Can be accessed using '{pageTitle}' inside your @Layout.hs@.
--
-- Example:
--
-- > action ShowProjectAction { projectId } = do
-- >     project <- fetch projectId
-- >     setTitle (project.title)
-- >
--
-- Inside your layout use it like:
--
-- > defaultLayout :: Html -> Html
-- > defaultLayout inner = [hsx|
-- > <head>
-- >     <title>{pageTitle}</title>
-- > </head>
-- > |]
--
setTitle :: (?request :: Request) => Text -> IO ()
setTitle title = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { title = Just (PageTitle title) })

setDescription :: (?request :: Request) => Text -> IO ()
setDescription description = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { description = Just (PageDescription description) })

setOGTitle :: (?request :: Request) => Text -> IO ()
setOGTitle title = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { ogTitle = Just (OGTitle title) })

setOGType :: (?request :: Request) => Text -> IO ()
setOGType type_ = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { ogType = Just (OGType type_) })

setOGDescription :: (?request :: Request) => Text -> IO ()
setOGDescription description = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { ogDescription = Just (OGDescription description) })

setOGUrl :: (?request :: Request) => Text -> IO ()
setOGUrl url = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { ogUrl = Just (OGUrl url) })

setOGImage :: (?request :: Request) => Text -> IO ()
setOGImage image = modifyIORef' (lookupPageHeadVault pageHeadVaultKey ?request) (\s -> s { ogImage = Just (OGImage image) })
