{-|
Module: IHP.PageTitle.ViewFunctions
Description: Manage the @<title>@ of your HTML pages
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageTitle.ViewFunctions
( pageTitle
, pageTitleOrDefault
, pageTitleOrNothing
, module IHP.PageTitle.ControllerFunctions -- | Re-export as we want to call setTitle from the beforeRender hook
) where

import IHP.Prelude
import IHP.PageTitle.Types
import IHP.Controller.Context
import IHP.PageTitle.ControllerFunctions


-- | Returns the current page title. The title can be set using @setTitle "my title"@ from the action.
--
-- If the title hasn't been set yet, this will return an empty string. You can also use 'pageTitleOrDefault' to pass a custom default title.
--
-- You can use this inside your @<title>@ tag like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >     </head>
-- > |]
--
--
-- *App-wide default title:*
--
-- You can set a app-wide default title by calling 'setTitle' from the @FrontController.hs@:
--
-- > instance InitControllerContext Web where
-- >     initContext = do
-- >         setLayout defaultLayout
-- >         initAutoRefresh
-- >         setTitle "Jobs"
--
--
-- *View-specific title:*
-- 
-- You can set a custom title inside the view by calling 'setTitle' inside the 'beforeRender' hook.
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setTitle "Custom title"
-- > 
-- >     html ShowView { .. } = [hsx|..|]
pageTitle :: (?context :: ControllerContext) => Text
pageTitle = pageTitleOrDefault ""

-- | Returns the current page title, like 'pageTitle' but returns a provided default value instead of an empty string if no title is set.
--
-- You can use this inside your @<title>@ tag like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitleOrDefault "My Application"}</title>
-- >     </head>
-- > |]
pageTitleOrDefault :: (?context :: ControllerContext) => Text -> Text
pageTitleOrDefault defaultValue = pageTitleOrNothing |> fromMaybe defaultValue

-- | Returns the current page title or Nothing if not set yet
pageTitleOrNothing :: (?context :: ControllerContext) => Maybe Text
pageTitleOrNothing = case maybeFromFrozenContext @PageTitle of
        Just (PageTitle title) -> Just title
        Nothing -> Nothing
