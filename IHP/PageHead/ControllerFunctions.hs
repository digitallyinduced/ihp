{-|
Module: IHP.PageHead.ControllerFunctions
Description: Manage the @<title>@ and @<meta>@ tags of your HTML pages
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.ControllerFunctions
( setTitle
, setOGTitle
, setOGType
, setOGDescription
, setOGUrl
, setOGImage
) where

import IHP.Prelude
import IHP.PageHead.Types
import IHP.Controller.Context

-- | Sets the page title. Can be accessed using '{pageTitle}' inside your @Layout.hs@.
--
-- Example:
--
-- > action ShowProjectAction { projectId } = do
-- >     project <- fetch projectId
-- >     setTitle (get #title project)
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
setTitle :: (?context :: ControllerContext) => Text -> IO ()
setTitle title = putContext (PageTitle title)

setOGTitle :: (?context :: ControllerContext) => Text -> IO ()
setOGTitle title = putContext (OGTitle title)

setOGType :: (?context :: ControllerContext) => Text -> IO ()
setOGType type_ = putContext (OGType type_)

setOGDescription :: (?context :: ControllerContext) => Text -> IO ()
setOGDescription description = putContext (OGDescription description)

setOGUrl :: (?context :: ControllerContext) => Text -> IO ()
setOGUrl url = putContext (OGUrl url)

setOGImage :: (?context :: ControllerContext) => Text -> IO ()
setOGImage image = putContext (OGImage image)
