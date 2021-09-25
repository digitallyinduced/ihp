{-|
Module: IHP.PageHead.ViewFunctions
Description: Manage the @<title>@ and @<meta>@ tags of your HTML pages
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.ViewFunctions
( pageTitle
, pageTitleOrDefault
, pageTitleOrNothing
, ogTitleOrDefault
, ogTypeOrDefault
, ogDescriptionOrDefault
, ogUrl
, ogImage
, module IHP.PageHead.ControllerFunctions -- | Re-export as we want to call setTitle from the beforeRender hook
) where

import IHP.Prelude
import IHP.PageHead.Types
import IHP.Controller.Context
import IHP.PageHead.ControllerFunctions
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html5 (Html)

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


-- | Returns the meta og:title element. The og:title can be set using @setOGTitle "my title"@ from the view.
--
-- You can use this inside your Layout like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >         {ogTitleOrDefault "default title"}
-- >     </head>
-- > |]
--
--
-- *View-specific og:title:*
-- 
-- You can override the default og:title inside the view by calling 'setOGTitle' inside the 'beforeRender' hook:
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setOGTitle "Custom title"
-- > 
-- >     html ShowView { .. } = [hsx|..|]
ogTitleOrDefault :: (?context :: ControllerContext) => Text -> Html
ogTitleOrDefault defaultValue = [hsx|<meta property="og:title" content={content}/>|]
    where
        content = case maybeFromFrozenContext @OGTitle of
            Just (OGTitle title) -> title
            Nothing -> defaultValue


-- | Returns the meta og:type element. The og:type can be set using @setOGType "data"@ from the view.
--
-- You can use this inside your Layout like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >         {ogTypeOrDefault "data"}
-- >     </head>
-- > |]
--
--
-- *View-specific og:type:*
-- 
-- You can override the default og:type inside the view by calling 'setOGType' inside the 'beforeRender' hook:
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setOGType "mytype"
-- > 
-- >     html ShowView { .. } = [hsx|..|]
ogTypeOrDefault :: (?context :: ControllerContext) => Text -> Html
ogTypeOrDefault defaultValue = [hsx|<meta property="og:type" content={content}/>|]
    where
        content = case maybeFromFrozenContext @OGType of
            Just (OGType type_) -> type_
            Nothing -> defaultValue

-- | Returns the meta og:description element. The og:description can be set using @setOGDescription "my description"@ from the view.
--
-- You can use this inside your Layout like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >         {ogDescriptionOrDefault "CO2 Database"}
-- >     </head>
-- > |]
--
--
-- *View-specific og:description:*
-- 
-- You can override the default og:description inside the view by calling 'setOGDescription' inside the 'beforeRender' hook:
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setOGDescription "The CO2 Footprint of beef is about 67kg CO2 per 1kg of beef."
-- > 
-- >     html ShowView { .. } = [hsx|..|]
ogDescriptionOrDefault :: (?context :: ControllerContext) => Text -> Html
ogDescriptionOrDefault defaultValue = [hsx|<meta property="og:description" content={content}/>|]
    where
        content = case maybeFromFrozenContext @OGDescription of
            Just (OGDescription description) -> description
            Nothing -> defaultValue


-- | Returns the meta og:url element if @setOGUrl "https://example.com/"@ was called before.
--
-- You can use this inside your Layout like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >         {ogUrl}
-- >     </head>
-- > |]
--
-- When 'setOGUrl' is not called, no meta tag will be rendered.
--
-- *Setting og:url:*
-- 
-- You can set the og:url inside the view by calling 'setOGUrl' inside the 'beforeRender' hook:
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setOGUrl (urlTo ShowAction { .. })
-- > 
-- >     html ShowView { .. } = [hsx|..|]
ogUrl :: (?context :: ControllerContext) => Text -> Html
ogUrl defaultValue = case maybeFromFrozenContext @OGUrl of
    Just (OGUrl url) -> [hsx|<meta property="og:url" content={url}/>|]
    Nothing -> mempty


-- | Returns the meta og:image element if @setOGImage "https://example.com/image.png"@ was called before.
--
-- You can use this inside your Layout like this:
--
-- > [hsx|
-- >     <head>
-- >         <title>{pageTitle}</title>
-- >         {ogImage}
-- >     </head>
-- > |]
--
-- When 'setOGImage' is not called, no meta tag will be rendered.
--
-- *Setting og:image:*
-- 
-- You can set the og:image inside the view by calling 'setOGImage' inside the 'beforeRender' hook:
--
-- > module JobSite.View.JobPositions.Show where
-- > 
-- > instance View ShowView where
-- >     beforeRender ShowView { .. } = do
-- >         setOGImage "https://example.com/image.png"
-- > 
-- >     html ShowView { .. } = [hsx|..|]
ogImage :: (?context :: ControllerContext) => Text -> Html
ogImage defaultValue = case maybeFromFrozenContext @OGImage of
    Just (OGImage url) -> [hsx|<meta property="og:image" content={url}/>|]
    Nothing -> mempty
