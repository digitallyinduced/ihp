{-|
Module: IHP.PageTitle.ControllerFunctions
Description: Manage the @<title>@ of your HTML pages
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageTitle.ControllerFunctions
( setTitle
) where

import IHP.Prelude
import IHP.PageTitle.Types
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
