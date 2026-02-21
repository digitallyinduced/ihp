{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-|
Module: IHP.ViewSupport
Description: Provides functions to be used in all views
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ViewSupport
( HtmlWithContext
, Layout
, Html
, View (..)
, currentViewId
, forEach
, isActivePath
, isActivePathOrSub
, css
, onClick
, onLoad
, theRequest
, ViewFetchHelpMessage
, param
, fetch
, query
, isActiveController
, isActiveAction
, nl2br
, stripTags
, theCSSFramework
, fromCSSFramework
, liveReloadWebsocketUrl
, assetPath
, assetVersion
) where

import IHP.Prelude
import qualified Text.Blaze.Html5 as Html5
import IHP.ControllerSupport
import IHP.ModelSupport
import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Text.Inflections as Inflector
import qualified Data.Either as Either
import GHC.TypeLits as T
import qualified Data.ByteString as ByteString
import IHP.Router.UrlGenerator (HasPath(..))
import Network.Wai
import Text.Blaze.Html5.Attributes as A
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml
import qualified Data.Sequences as Sequences
import qualified IHP.View.CSSFramework as CSSFramework ()
import IHP.View.Types
import qualified IHP.FrameworkConfig as FrameworkConfig
import qualified IHP.HSX.Attribute as HSX
import qualified Network.Wai.Middleware.AssetPath as AssetPath
import IHP.ActionType (isActiveController)

class View theView where
    -- | Hook which is called before the render is called
    beforeRender :: (?context :: ControllerContext, ?request :: Request) => theView -> IO ()
    beforeRender view = pure ()

    -- Renders the view as html
    html :: (?context :: ControllerContext, ?view :: theView, ?request :: Request) => theView -> Html5.Html

    -- | Renders the view to a JSON
    json :: theView -> JSON.Value
    json = error "Json View for this route is not implemented"

-- | Returns a string to be used as a html id attribute for the current view.
-- E.g. when calling @currentViewId@ while rendering the view @Web.View.Projects.Show@, this will return @"projects-show"@
--
-- Useful to automatically scope certain css rules to a specific view.
-- Example:
--
-- > module Web.View.Projects.Show where
-- > render = [hsx|<div id={currentViewId}>Hello World!</div>|]
--
-- This will render @<div id="projects-show">Hello World!</div>@
{-# INLINE currentViewId #-}
currentViewId :: (?view :: view, Typeable view) => Text
currentViewId =
        case moduleParts of
            [_, "View", controllerName, viewName] -> Either.fromRight (error "currentViewId: Failed to parse controller name") (Inflector.toDashed controllerName) <> "-" <> Either.fromRight (error "currentViewId: Failed to parse view name") (Inflector.toDashed viewName)
            _ -> error ("currentViewId: Failed to read view id for " <> cs moduleName)
    where
        constructor = Typeable.typeRepTyCon (Typeable.typeOf ?view)

        -- Module name: Web.View.Projects.Show
        moduleName :: Text
        moduleName = cs (Typeable.tyConModule constructor)

        moduleParts :: [Text]
        moduleParts = Text.splitOn "." moduleName

-- | Returns @True@ when the current request path matches the path given as argument. Takes into account the search query: @?name=value@
--
-- __Example:__ The browser has requested the url @\/Projects@.
--
-- >>> isActivePath "/Projects"
-- True
--
-- Returns @True@ because @"\/Projects"@ is the current requested path.
--
-- >>> isActivePath "/Users"
-- False
--
-- Returns false because @"/Users"@ is not @"/Projects"@
--
-- __Example:__ The browser has requested the url @\/Projects@.
--
-- >>> isActivePath "/Projects/1"
-- False
--
-- This function returns @False@ when a sub-path is request. Use 'isActivePathOrSub' if you want this example to return @True@.
isActivePath :: (?request :: Request, PathString controller) => controller -> Bool
isActivePath route =
    let
        currentPath = theRequest.rawPathInfo <> theRequest.rawQueryString
    in
        currentPath == cs (pathToString route)

-- | Returns @True@ when the current request path starts with the path given as argument.
--
-- __Example:__ The browser has requested the url @\/Projects/1@.
--
-- >>> isActivePathOrSub "/Projects"
-- True
--
-- __Example:__ The browser has requested the url @\/Projects@.
--
-- >>> isActivePathOrSub "/Projects"
-- True
--
-- Also see 'isActivePath'.
isActivePathOrSub :: (?request :: Request, PathString controller) => controller -> Bool
isActivePathOrSub route =
    let
        currentPath = theRequest.rawPathInfo
    in
        cs (pathToString route) `ByteString.isPrefixOf` currentPath

-- | Returns @True@ when the given action matches the path of the currently executed action
--
-- __Example:__ The browser has requested @\/PostsAction@.
--
-- >>> isActiveAction PostsAction
-- True

-- __Example:__ The browser has requested @\/ShowPossAction@ along with the post ID.
--
-- >>> -- Get the post ID out of a UUID.
-- >>> let myUUID = ...
-- >>> let postId = (Id myUUID) :: Id Post
-- >>> isActiveAction (ShowPostAction postId)
-- True
--
isActiveAction :: forall controllerAction. (?request :: Request, HasPath controllerAction) => controllerAction -> Bool
isActiveAction controllerAction =
    isActivePath (pathTo controllerAction)

css = plain

onClick = A.onclick
onLoad = A.onload

-- | Returns the current request
theRequest :: (?request :: Request) => Request
theRequest = ?request
{-# INLINE theRequest #-}

class PathString a where
    pathToString :: a -> Text

instance PathString Text where
    pathToString path = path

instance {-# OVERLAPPABLE #-} HasPath action => PathString action where
    pathToString = pathTo

-- | This class provides helpful compile-time error messages when you use common
-- controller functions inside of your views.
class ViewParamHelpMessage where
    param :: a

instance (T.TypeError (T.Text "‘param‘ can only be used inside your controller actions.\nYou have to run the ‘param \"my_param\"‘ call inside your controller and then pass the resulting value to your view.\n\nController Example:\n\n    module Web.Controller.Projects\n\n    instance Controller ProjectsController where\n        action ProjectsAction = do\n            let showDetails = param \"showDetails\"\n            render ProjectsView { showDetails }\n\nView Example:\n\n    module Web.View.Projects.Index\n\n    data ProjectsView = ProjectsView { showDetails :: Bool }\n    instance View ProjectsView where\n        html ProjectsView { .. } = [hsx|Show details: {showDetails}|]\n\n")) => ViewParamHelpMessage where
    param = error "unreachable"

-- | This class provides helpful compile-time error messages when you use common
-- controller functions inside of your views.
class ViewFetchHelpMessage where
    fetch :: a
    query :: a
instance (T.TypeError (T.Text "‘fetch‘ or ‘query‘ can only be used inside your controller actions. You have to call it from your controller action and then pass the result to the view.")) => ViewFetchHelpMessage where
    fetch = error "unreachable"
    query = error "unreachable"

-- Note: The type error instance for (Id' record -> controller) was removed
-- because Id' is now a type family. Per-table Id newtypes (UserId, etc.)
-- provide clear error messages naturally.

fromCSSFramework :: (?request :: Request, KnownSymbol field, HasField field CSSFramework (CSSFramework -> appliedFunction)) => Proxy field -> appliedFunction
fromCSSFramework field = let cssFramework = theCSSFramework in (get field cssFramework) cssFramework

theCSSFramework :: (?request :: Request) => CSSFramework
theCSSFramework = ?request.frameworkConfig.cssFramework

-- | Replaces all newline characters with a @<br>@ tag. Useful for displaying preformatted text.
--
-- >>> nl2br "Hello\nWorld!"
-- [hsx|Hello<br/>World!|]
nl2br :: (Sequences.Textual text, ToHtml text) => text -> Html5.Html
nl2br content = content
    |> Sequences.lines
    |> map (\line -> [hsx|{line}<br/>|])
    |> mconcat

type Html = HtmlWithContext ControllerContext

-- | The URL for the dev-mode live reload server. Typically "ws://localhost:8001"
liveReloadWebsocketUrl :: (?request :: Request) => Text
liveReloadWebsocketUrl = ?request.frameworkConfig.ideBaseUrl
    |> Text.replace "http://" "ws://"
    |> Text.replace "https://" "wss://"

-- Note: Per-table ApplyAttribute instances for Id newtypes (UserId, ProjectId, etc.)
-- are generated by the schema compiler in Generated.ActualTypes.PrimaryKeys.


-- | Adds a cache buster to a asset path
--
-- >>> assetPath "/keyhandlers.js"
-- "/keyhandlers.js?v=9be8995c-7055-43d9-a1b2-43e05c210271"
--
-- The asset version can be configured using the
-- @IHP_ASSET_VERSION@ environment variable.
assetPath :: (?request :: Request) => Text -> Text
assetPath assetPath = AssetPath.assetPath theRequest assetPath

-- | Returns the assetVersion
--
-- >>> assetVersion
-- "9be8995c-7055-43d9-a1b2-43e05c210271"
--
-- The asset version can be configured using the
-- @IHP_ASSET_VERSION@ environment variable.
assetVersion :: (?request :: Request) => Text
assetVersion = fromMaybe (error "assetPath middleware missing") (AssetPath.assetVersion theRequest)