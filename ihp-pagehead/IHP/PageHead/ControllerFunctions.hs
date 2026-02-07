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
import Data.IORef (writeIORef)
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
setTitle title = writeIORef (lookupPageHeadVault pageTitleVaultKey ?request) (Just (PageTitle title))

setDescription :: (?request :: Request) => Text -> IO ()
setDescription description = writeIORef (lookupPageHeadVault pageDescriptionVaultKey ?request) (Just (PageDescription description))

setOGTitle :: (?request :: Request) => Text -> IO ()
setOGTitle title = writeIORef (lookupPageHeadVault ogTitleVaultKey ?request) (Just (OGTitle title))

setOGType :: (?request :: Request) => Text -> IO ()
setOGType type_ = writeIORef (lookupPageHeadVault ogTypeVaultKey ?request) (Just (OGType type_))

setOGDescription :: (?request :: Request) => Text -> IO ()
setOGDescription description = writeIORef (lookupPageHeadVault ogDescriptionVaultKey ?request) (Just (OGDescription description))

setOGUrl :: (?request :: Request) => Text -> IO ()
setOGUrl url = writeIORef (lookupPageHeadVault ogUrlVaultKey ?request) (Just (OGUrl url))

setOGImage :: (?request :: Request) => Text -> IO ()
setOGImage image = writeIORef (lookupPageHeadVault ogImageVaultKey ?request) (Just (OGImage image))
