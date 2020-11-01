{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.ViewSupport
Description: Provides functions to be used in all views
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ViewSupport
( HtmlWithContext
, CreateViewContext (..)
, Layout
, View (..)
, currentViewId
, forEach
, isActivePath
, isActivePathOrSub
, css
, onClick
, onLoad
, theRequest
, viewContext
, addStyle
, ViewFetchHelpMessage
, param
, fetch
, query
, isActiveController
, renderFlashMessages
, nl2br
, stripTags
) where

import IHP.Prelude
import qualified Text.Blaze
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
import IHP.RouterSupport hiding (get)
import qualified Network.Wai as Wai
import Text.Blaze.Html5.Attributes as A
import qualified IHP.ControllerSupport as ControllerSupport
import qualified IHP.Controller.Session as Session
import IHP.HtmlSupport.QQ (hsx)
import IHP.HtmlSupport.ToHtml
import qualified Data.Sequences as Sequences
import qualified IHP.Controller.RequestContext
import qualified IHP.View.CSSFramework as CSSFramework
import IHP.View.Types
import qualified IHP.FrameworkConfig as FrameworkConfig

type HtmlWithContext context = (?context :: context) => Html5.Html

-- | A layout is just a function taking a view and returning a new view.
--
-- __Example:__ A very basic html layout.
-- 
-- > myLayout :: Layout
-- > myLayout view = [hsx|
-- >     <html>
-- >         <body>
-- >             {view}
-- >         </body>
-- >     </html>
-- > |]
type Layout = Html5.Html -> Html5.Html

class CreateViewContext viewContext where
    type ViewApp viewContext
    createViewContext :: (?context :: RequestContext, ?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => IO viewContext



class View theView viewContext | theView -> viewContext where
    beforeRender :: (?context :: viewContext) => (viewContext, theView) -> (viewContext, theView)
    {-# INLINE beforeRender #-}
    beforeRender view = view
    html :: (?context :: viewContext, ?view :: theView) => theView -> Html5.Html
    json :: theView -> JSON.Value
    json = error "Not implemented"

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

-- | Returns @True@ when the current request path matches the path given as argument.
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
-- This function returns @False@ when a sub-path is request. Uss 'isActivePathOrSub' if you want this example to return @True@.
isActivePath :: (?context :: viewContext, HasField "requestContext" viewContext RequestContext, PathString controller) => controller -> Bool
isActivePath route =
    let 
        currentPath = Wai.rawPathInfo theRequest
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
isActivePathOrSub :: (?context :: viewContext, HasField "requestContext" viewContext RequestContext, PathString controller) => controller -> Bool
isActivePathOrSub route =
    let
        currentPath = Wai.rawPathInfo theRequest
    in
        cs (pathToString route) `ByteString.isPrefixOf` currentPath

-- | Returns @True@ when the given type matches the type of the currently executed controller action
--
-- __Example:__ The browser has requested @\/Posts@ and the @Posts@ action of the @PostsController@ is called.
--
-- >>> isActiveController @PostsController
-- True
--
-- Returns @True@ because the current action is part of the @PostsController@
isActiveController :: forall controller viewContext. (?context :: viewContext, HasField "controllerContext" viewContext ControllerSupport.ControllerContext, Typeable controller) => Bool
isActiveController =
    let
        ?controllerContext = ?context |> getField @"controllerContext"
    in
        let
            (ActionType actionType) = fromControllerContext @ControllerSupport.ActionType
        in
            (Typeable.typeRep @Proxy @controller (Proxy @controller)) == actionType


css = plain

onClick = A.onclick
onLoad = A.onload

-- | Returns the current request
theRequest :: (?context :: viewContext, HasField "requestContext" viewContext RequestContext) => Wai.Request
theRequest = 
    let
        requestContext = getField @"requestContext" ?context
        request = getField @"request" requestContext
    in request
{-# INLINE theRequest #-}

class PathString a where
    pathToString :: a -> Text

instance PathString Text where
    pathToString path = path

instance {-# OVERLAPPABLE #-} HasPath action => PathString action where
    pathToString = pathTo

-- | Alias for @?context@
viewContext :: (?context :: viewContext) => viewContext
viewContext = ?context
{-# INLINE viewContext #-}

-- | Adds an inline style element to the html.
--
-- This helps to work around the issue, that our HSX parser cannot deal with CSS yet.
--
-- __Example:__
--
-- > myStyle = addStyle "#my-div { color: blue; }"
-- > [hsx|{myStyle}<div id="my-div">Hello World</div>|]
--
-- This will render like:
--
-- > <style>
-- >     #my-div { color: blue; }
-- > </style>
-- > <div id="my-div">Hello World</div>
addStyle :: (ConvertibleStrings string Text) => string -> Html5.Markup
addStyle style = Html5.style (Html5.preEscapedText (cs style))
{-# INLINE addStyle #-}

-- | This class provides helpful compile-time error messages when you use common
-- controller functions inside of your views.
class ViewParamHelpMessage where
    param :: a

instance (T.TypeError (T.Text "‘param‘ can only be used inside your controller actions.\nYou have to run the ‘param \"my_param\"‘ call inside your controller and then pass the resulting value to your view.\n\nController Example:\n\n    module Web.Controller.Projects\n\n    instance Controller ProjectsController where\n        action ProjectsAction = do\n            let showDetails = param \"showDetails\"\n            render ProjectsView { showDetails }\n\nView Example:\n\n    module Web.View.Projects.Index\n\n    data ProjectsView = ProjectsView { showDetails :: Bool }\n    instance View ProjectsView ViewContext where\n        html ProjectsView { .. } = [hsx|Show details: {showDetails}|]\n\n")) => ViewParamHelpMessage where
    param = error "unreachable"

-- | This class provides helpful compile-time error messages when you use common
-- controller functions inside of your views.
class ViewFetchHelpMessage where
    fetch :: a
    query :: a
instance (T.TypeError (T.Text "‘fetch‘ or ‘query‘ can only be used inside your controller actions. You have to call it from your controller action and then pass the result to the view.")) => ViewFetchHelpMessage where
    fetch = error "unreachable"
    query = error "unreachable"

instance (T.TypeError (T.Text "Looks like you forgot to pass a " :<>: (T.ShowType (GetModelByTableName record)) :<>: T.Text " id to this data constructor.")) => Eq (Id' (record :: T.Symbol) -> controller) where
    a == b = error "unreachable"

-- | Displays the flash messages for the current request.
--
-- You can add a flash message to the next request by calling 'IHP.Controller.Session.setSuccessMessage' or 'IHP.Controller.Session.setErrorMessage':
--
-- > action CreateProjectAction = do
-- >     ...
-- >     setSuccessMessage "Your project has been created successfully"
-- >     redirectTo ShowProjectAction { .. }
--
--
-- > action CreateTeamAction = do
-- >     unless userOnPaidPlan do
-- >         setErrorMessage "This requires you to be on the paid plan"
-- >         redirectTo NewTeamAction
-- >
-- >     ...
--
-- For success messages, the text message is wrapped in a @<div class="alert alert-success">...</div>@, which is automatically styled by bootstrap.
-- Errors flash messages are wraped in @<div class="alert alert-danger">...</div>@.
renderFlashMessages :: forall viewContext. (?context :: viewContext, HasField "flashMessages" viewContext [Session.FlashMessage], HasField "cssFramework" viewContext CSSFramework) => Html5.Html
renderFlashMessages = render flashMessages
    where
        flashMessages :: [Session.FlashMessage]
        flashMessages = (getField @"flashMessages" ?viewContext)

        render :: [Session.FlashMessage] -> Html5.Html
        render = getCurrentCSSFramework #styledFlashMessages

getCurrentCSSFramework :: (?viewContext :: viewContext, HasField "cssFramework" viewContext CSSFramework, KnownSymbol field, HasField field CSSFramework (CSSFramework -> appliedFunction)) => Proxy field -> appliedFunction
getCurrentCSSFramework field = let cssFramework = (get #cssFramework ?context) in (get field cssFramework) cssFramework

-- | Replaces all newline characters with a @<br>@ tag. Useful for displaying preformatted text.
--
-- >>> nl2br "Hello\nWorld!"
-- [hsx|Hello<br/>World!|]
nl2br :: (Sequences.Textual text, ToHtml text) => text -> Html5.Html
nl2br content = content
    |> Sequences.lines
    |> map (\line -> [hsx|{line}<br/>|])
    |> mconcat

instance {-# OVERLAPPABLE #-} HasField "requestContext" viewContext RequestContext => FrameworkConfig.ConfigProvider viewContext where
    getFrameworkConfig viewContext = viewContext
            |> get #requestContext
            |> get #frameworkConfig