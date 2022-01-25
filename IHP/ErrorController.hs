{-|
Module: IHP.ErrorController
Description:  Provides web-based error screens for runtime errors in IHP
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ErrorController
( displayException
, handleNoResponseReturned
, handleNotFound
, handleRouterException
, buildNotFoundResponse
) where

import IHP.Prelude hiding (displayException)
import qualified IHP.Controller.Param as Param
import qualified IHP.Router.Types as Router
import qualified Network.HTTP.Types.Method as Router
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import IHP.Controller.RequestContext
import Network.HTTP.Types (status500, status404, status400)
import Network.Wai
import Network.HTTP.Types.Header

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Database.PostgreSQL.Simple as PG
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LBS

import IHP.HSX.QQ (hsx)
import qualified IHP.ModelSupport as ModelSupport
import IHP.FrameworkConfig
import qualified IHP.Environment as Environment
import IHP.Controller.Context
import qualified System.Directory as Directory
import IHP.ApplicationContext

handleNoResponseReturned :: (Show controller, ?context :: ControllerContext) => controller -> IO ResponseReceived
handleNoResponseReturned controller = do
    let codeSample :: Text = "render MyView { .. }"
    let errorMessage = [hsx|
            <h2>Possible Solutions</h2>
            <p>You can fix this by calling '{codeSample}' at the end of your action.</p>

            <h2>Details</h2>
            <p style="font-size: 16px">No response was returned while running the action {tshow controller}</p>

        |]
    let title = [hsx|No response returned in {tshow controller}|]
    let RequestContext { respond } = get #requestContext ?context
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

-- | Renders a 404 not found response. If a static/404.html exists, that is rendered instead of the IHP not found page
handleNotFound :: (?context :: RequestContext) => IO ResponseReceived
handleNotFound = do
    response <- buildNotFoundResponse
    let RequestContext { respond } = ?context
    respond response

buildNotFoundResponse :: forall context. (?context :: context, ConfigProvider context) => IO Response
buildNotFoundResponse = do
    hasCustomNotFound <- Directory.doesFileExist "static/404.html"
    if hasCustomNotFound
        then customNotFoundResponse
        else pure defaultNotFoundResponse

-- | The default IHP 404 not found page
defaultNotFoundResponse :: forall context. (?context :: context, ConfigProvider context) => Response
defaultNotFoundResponse = responseBuilder status404 [(hContentType, "text/html")] $ Blaze.renderHtmlBuilder $ H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>

    <title>Action not found</title>
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@500;900&display=swap');
    html, body {
        height: 100%;
        font-family: 'Poppins', sans-serif;
    }
    body {
        color: white;
        letter-spacing: 1.01px;
        text-align: center;
        background-image: linear-gradient(59deg, #044d60 3%, #0082a3 98%);
        display: grid;
        place-items: center;
        margin: 0;
    }
    div {
        width: 796px;
        max-width: 90vw;
    }
    hr {
        border: none;
        border-top: 1px solid white;
    }
    h1 {
        font-weight: 900;
        margin: 24px 0 12px 0;
        font-size: 31px;
    }
    p {
        font-weight: 500;
        margin: 0;
        font-size: 31px;
    }
    </style>
</head>
<body>
    <div>
        <svg width="229px" height="124px" viewBox="0 0 458 248">
            <defs>
                <filter color-interpolation-filters="auto" id="filter-1">
                    <feColorMatrix in="SourceGraphic" type="matrix" values="0 0 0 0 1.000000 0 0 0 0 1.000000 0 0 0 0 1.000000 0 0 0 1.000000 0"></feColorMatrix>
                </filter>
            </defs>
            <g id="Logo-Showcase" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                <g filter="url(#filter-1)" id="Group-2">
                    <g>
                        <g id="Group">
                            <path d="M66.05902,0 L104.505158,0 C105.28742,2.63545372e-15 105.997877,0.456069021 106.323569,1.1673062 L194.702842,194.167306 C195.162726,195.171587 194.721406,196.358528 193.717125,196.818412 C193.45583,196.938065 193.171818,197 192.884431,197 L150.564632,197 C149.770251,197 149.051173,196.529864 148.732554,195.80218 L64.2269428,2.80218032 C63.7839108,1.79035204 64.2450114,0.610954791 65.2568397,0.167922835 C65.5097746,0.057174527 65.7829017,-3.93367112e-16 66.05902,0 Z" id="Path-4" fill="#026B86"></path>
                            <path d="M65.8632635,98 L103.588393,98 C105.245247,98 106.588393,99.3431458 106.588393,101 C106.588393,101.562078 106.430486,102.11285 106.13267,102.589544 L48.0306793,195.589544 C47.4825198,196.466947 46.5209615,197 45.4864016,197 L5.52260972,197 C3.86575547,197 2.52260972,195.656854 2.52260972,194 C2.52260972,193.420447 2.69047977,192.8533 3.00592763,192.367116 L63.3465814,99.3671157 C63.8997523,98.5145414 64.846956,98 65.8632635,98 Z" id="Path-2" fill="#063642"></path>
                        </g>
                        <path d="M239.055,197 L239.055,98.588 L215.147,98.588 L215.147,197 L239.055,197 Z M287.6355,197 L287.6355,155.856 L325.9995,155.856 L325.9995,197 L349.7685,197 L349.7685,98.588 L325.9995,98.588 L325.9995,135.84 L287.6355,135.84 L287.6355,98.588 L263.8665,98.588 L263.8665,197 L287.6355,197 Z M398.349,197 L398.349,159.887 L410.164,159.887 C415.260667,159.887 420.172,159.331 424.898,158.219 C429.624,157.107 433.794,155.346333 437.408,152.937 C441.022,150.527667 443.894667,147.353833 446.026,143.4155 C448.157333,139.477167 449.223,134.635333 449.223,128.89 C449.223,123.237333 448.226833,118.488167 446.2345,114.6425 C444.242167,110.796833 441.5085,107.6925 438.0335,105.3295 C434.5585,102.9665 430.481167,101.252167 425.8015,100.1865 C421.121833,99.1208333 416.094667,98.588 410.72,98.588 L410.72,98.588 L374.58,98.588 L374.58,197 L398.349,197 Z M408.357,140.983 L398.349,140.983 L398.349,117.77 L408.774,117.77 C410.905333,117.77 412.967167,117.909 414.9595,118.187 C416.951833,118.465 418.735667,119.021 420.311,119.855 C421.886333,120.689 423.137333,121.847333 424.064,123.33 C424.990667,124.812667 425.454,126.712333 425.454,129.029 C425.454,131.345667 424.990667,133.2685 424.064,134.7975 C423.137333,136.3265 421.863167,137.554333 420.2415,138.481 C418.619833,139.407667 416.789667,140.056333 414.751,140.427 C412.712333,140.797667 410.581,140.983 408.357,140.983 L408.357,140.983 Z" id="IHP" fill="#000000" fill-rule="nonzero"></path>
                    </g>
                </g>
            </g>
        </svg>
        <hr />
        <h1>Error 404</h1>
        <p>Action not found</p>
    </div>
</body>
    |]

-- | Renders the static/404.html file
customNotFoundResponse :: IO Response
customNotFoundResponse = do
    -- We cannot use responseFile here as responseFile ignore the status code by default
    --
    -- See https://github.com/yesodweb/wai/issues/644
    page <- LBS.readFile "static/404.html"
    pure $ responseLBS status404 [(hContentType, "text/html")] page

displayException :: (Show action, ?context :: ControllerContext, ?applicationContext :: ApplicationContext, ?requestContext :: RequestContext) => SomeException -> action -> Text -> IO ResponseReceived
displayException exception action additionalInfo = do
    -- Dev handlers display helpful tips on how to resolve the problem
    let devHandlers =
            [ postgresHandler
            , paramNotFoundExceptionHandler
            , patternMatchFailureHandler
            , recordNotFoundExceptionHandlerDev
            ]

    -- Prod handlers should not leak any information about the system
    let prodHandlers =
            [ recordNotFoundExceptionHandlerProd
            ]

    let allHandlers = if fromConfig environment == Environment.Development
            then devHandlers
            else prodHandlers

    let supportingHandlers = allHandlers |> mapMaybe (\f -> f exception action additionalInfo)

    let displayGenericError = genericHandler exception action additionalInfo


    -- Additionally to rendering the error message to the browser we also send it
    -- to the error tracking service (e.g. sentry). Usually this service also writes
    -- the error message to the stderr output
    --
    let exceptionTracker = ?applicationContext
            |> get #frameworkConfig
            |> get #exceptionTracker
            |> get #onException
    let request = get #request ?requestContext


    exceptionTracker (Just request) exception

    supportingHandlers
        |> head
        |> fromMaybe displayGenericError

-- | Responds to all exceptions with a generic error message.
--
-- In dev mode the action and exception is added to the output.
-- In production mode nothing is specific is communicated about the exception
genericHandler :: (Show controller, ?context :: ControllerContext) => Exception.SomeException -> controller -> Text -> IO ResponseReceived
genericHandler exception controller additionalInfo = do
    let devErrorMessage = [hsx|An exception was raised while running the action {tshow controller}{additionalInfo}|]
    let devTitle = [hsx|{Exception.displayException exception}|]

    let prodErrorMessage = [hsx|An exception was raised while running the action|]
    let prodTitle = [hsx|An error happened|]

    let (errorMessage, errorTitle) = if fromConfig environment == Environment.Development
            then (devErrorMessage, devTitle)
            else (prodErrorMessage, prodTitle)
    let RequestContext { respond } = get #requestContext ?context

    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError errorTitle errorMessage))

postgresHandler :: (Show controller, ?context :: ControllerContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
postgresHandler exception controller additionalInfo = do
    let
        handlePostgresOutdatedError :: Show exception => exception -> H.Html -> IO ResponseReceived
        handlePostgresOutdatedError exception errorText = do
            let ihpIdeBaseUrl = ?context
                    |> getFrameworkConfig
                    |> get #ideBaseUrl
            let title = [hsx|Database looks outdated. {errorText}|]
            let errorMessage = [hsx|
                        <h2>Possible Solutions</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            Have you clicked on
                            <form method="POST" action={ihpIdeBaseUrl <> "/NewMigration"} target="_blank" style="display: inline">
                                <button type="submit">Migrate DB</button>
                            </form>
                            after updating the Schema?
                        </div>

                        <h2>Details</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

        handleSqlError :: ModelSupport.EnhancedSqlError -> IO ResponseReceived
        handleSqlError exception = do
            let ihpIdeBaseUrl = ?context
                    |> getFrameworkConfig
                    |> get #ideBaseUrl
            let sqlError = get #sqlError exception
            let title = [hsx|{get #sqlErrorMsg sqlError}|]
            let errorMessage = [hsx|
                        <h2>While running the following Query:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <code>{get #sqlErrorQuery exception}</code>
                        </div>

                        <h2>With Query Parameters:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <code>{get #sqlErrorQueryParams exception}</code>
                        </div>

                        <h2>Details:</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
    case fromException exception of
        Just (exception :: PG.ResultError) -> Just (handlePostgresOutdatedError exception "The database result does not match the expected type.")
        Nothing -> case fromException exception of
            -- Catching  `relation "..." does not exist`
            Just exception@ModelSupport.EnhancedSqlError { sqlError }
                |  "relation" `ByteString.isPrefixOf` (get #sqlErrorMsg sqlError)
                && "does not exist" `ByteString.isSuffixOf` (get #sqlErrorMsg sqlError)
                -> Just (handlePostgresOutdatedError exception "A table is missing.")
            
            -- Catching  `columns "..." does not exist`
            Just exception@ModelSupport.EnhancedSqlError { sqlError }
                |  "column" `ByteString.isPrefixOf` (get #sqlErrorMsg sqlError)
                && "does not exist" `ByteString.isSuffixOf` (get #sqlErrorMsg sqlError)
                -> Just (handlePostgresOutdatedError exception "A column is missing.")
            -- Catching other SQL Errors
            Just exception -> Just (handleSqlError exception)
            Nothing -> Nothing

patternMatchFailureHandler :: (Show controller, ?context :: ControllerContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
patternMatchFailureHandler exception controller additionalInfo = do
    case fromException exception of
        Just (exception :: Exception.PatternMatchFail) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)
            let errorMessage = [hsx|
                    <h2>Possible Solutions</h2>
                    <p>a) Maybe the action function is missing for {tshow controller}? You can fix this by adding an action handler like this to the controller '{controllerPath}':</p>
                    <pre>{codeSample}</pre>
                    <p style="margin-bottom: 2rem">b) A pattern match like 'let (Just value) = ...' failed. Please see the details section.</p>

                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]
                    where
                        codeSample = "    action (" <> tshow controller <> ") = do\n        renderPlain \"Hello World\""

            let title = [hsx|Pattern match failed while executing {tshow controller}|]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Nothing -> Nothing

-- Handler for 'IHP.Controller.Param.ParamNotFoundException'
paramNotFoundExceptionHandler :: (Show controller, ?context :: ControllerContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
paramNotFoundExceptionHandler exception controller additionalInfo = do
    case fromException exception of
        Just (exception@(Param.ParamNotFoundException paramName)) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)

            let renderParam (paramName, paramValue) = [hsx|<li>{paramName}: {paramValue}</li>|]
            let solutionHint =
                    if isEmpty Param.allParams
                        then [hsx|
                                This action was called without any parameters at all.
                                You can pass this parameter by appending <code>?{paramName}=someValue</code> to the URL.
                            |]
                        else [hsx|
                            <p>The following parameters are provided by the request:</p>
                            <ul>{forEach Param.allParams renderParam}</ul>

                            <p>a) Is there a typo in your call to <code>param {tshow paramName}</code>?</p>
                            <p>b) You can pass this parameter by appending <code>&{paramName}=someValue</code> to the URL.</p>
                            <p>c) You can pass this parameter using a form input like <code>{"<input type=\"text\" name=\"" <> paramName <> "\"/>" :: ByteString}</code>.</p>
                        |]
            let errorMessage = [hsx|
                    <h2>
                        This exception was caused by a call to <code>param {tshow paramName}</code> in {tshow controller}.
                    </h2>
                    <p>
                        A request parameter is just a query parameter like <code>/MyAction?someParameter=someValue&secondParameter=1</code>
                        or a form input when the request was submitted from a html form or via ajax.
                    </p>
                    <h2>Possible Solutions:</h2>
                    {solutionHint}

                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]



            let title = [hsx|Parameter <q>{paramName}</q> not found in the request|]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Just (exception@(Param.ParamCouldNotBeParsedException { name, parserError })) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)

            let renderParam (paramName, paramValue) = [hsx|<li>{paramName}: {paramValue}</li>|]
            let errorMessage = [hsx|
                    <h2>
                        This exception was caused by a call to <code>param {tshow name}</code> in {tshow controller}.
                    </h2>
                    <p>
                        Here's the error output from the parser: {parserError}
                    </p>

                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]



            let title = [hsx|Parameter <q>{name}</q> was invalid|]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Nothing -> Nothing

-- Handler for 'IHP.ModelSupport.RecordNotFoundException'
--
-- Used only in development mode of the app.
recordNotFoundExceptionHandlerDev :: (Show controller, ?context :: ControllerContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
recordNotFoundExceptionHandlerDev exception controller additionalInfo =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException { queryAndParams = (query, params) })) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)
            let errorMessage = [hsx|
                    <p>
                        The following SQL was executed:
                        <pre class="ihp-error-code">{query}</pre>
                    </p>
                    <p>
                        These query parameters have been used:
                        <pre class="ihp-error-code">{params}</pre>
                    </p>

                    <p>
                        This exception was caused by a call to <code>fetchOne</code> in {tshow controller}.
                    </p>

                    <h2>Possible Solutions:</h2>

                    <p>
                        a) Use <span class="ihp-error-inline-code">fetchOneOrNothing</span>. This will return a <span class="ihp-error-inline-code">Nothing</span>
                        when no results are returned by the database.
                    </p>

                    <p>
                        b) Make sure the the data you are querying is actually there.
                    </p>


                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]



            let title = [hsx|Call to fetchOne failed. No records returned.|]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Nothing -> Nothing

-- Handler for 'IHP.ModelSupport.RecordNotFoundException'
--
-- Used only in production mode of the app. The exception is handled by calling 'handleNotFound'
recordNotFoundExceptionHandlerProd :: (?context :: ControllerContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
recordNotFoundExceptionHandlerProd exception controller additionalInfo =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException {})) ->
            let requestContext = get #requestContext ?context
            in
                let ?context = requestContext
                in Just handleNotFound
        Nothing -> Nothing

handleRouterException :: (?context :: RequestContext) => SomeException -> IO ResponseReceived
handleRouterException exception =
    case fromException exception of
        Just Router.NoConstructorMatched { expectedType, value, field } -> do
            let errorMessage = [hsx|
                    <p>Routing failed with: {tshow exception}</p>

                    <h2>Possible Solutions</h2>
                    <p>You can pass this parameter by appending <code>&{field}=someValue</code> to the URL.</p>
                |]
            let title = case value of
                    Just value -> [hsx|Expected <strong>{expectedType}</strong> for field <strong>{field}</strong> but got <q>{value}</q>|]
                    Nothing -> [hsx|The action was called without the required <q>{field}</q> parameter|]
            let RequestContext { respond } = ?context
            respond $ responseBuilder status400 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Just Router.BadType { expectedType, value = Just value, field } -> do
            let errorMessage = [hsx|
                    <p>Routing failed with: {tshow exception}</p>
                |]
            let title = [hsx|Query parameter <q>{field}</q> needs to be a <q>{expectedType}</q> but got <q>{value}</q>|]
            let RequestContext { respond } = ?context
            respond $ responseBuilder status400 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        _ -> case fromException exception of
            Just Router.UnexpectedMethodException { allowedMethods = [Router.DELETE], method = Router.GET } -> do
                let exampleLink :: Text = "<a href={DeleteProjectAction} class=\"js-delete\">Delete Project</a>"
                let formExample :: Text = cs [plain|
<form method="POST" action={DeleteProjectAction}>
    <input type="hidden" name="_method" value="DELETE"/>
    <button type="submit">Delete Project</button>
</form>
                |]
                let errorMessage = [hsx|
                        <p>
                            You cannot directly link to Delete Action.
                            GET requests should not have any external side effects, as a user could accidentally trigger it by following a normal link.
                        </p>

                        <h2>Possible Solutions</h2>
                        <p>
                            a) Add a <code>js-delete</code> class to your link. IHP's helper.js will intercept link clicks on these links and use a form with a DELETE request to submit the request.
                            <br /><br/>

                            Example: <br /><br />
                            <code>{exampleLink}</code>
                        </p>
                        <p>
                            b) Use a form to submit the request as a DELETE request:
                            <br /><br/>

                            Example: <br />
                            <pre>{formExample}</pre>
                            HTML forms don't support DELETE requests natively, therefore we use the hidden input field to work around this browser limitation.
                        </p>
                    |]
                let title = [hsx|Action was called from a GET request, but needs to be called as a DELETE request|]
                let RequestContext { respond } = ?context
                respond $ responseBuilder status400 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
            Just Router.UnexpectedMethodException { allowedMethods = [Router.POST], method = Router.GET } -> do
                let errorMessage = [hsx|
                        <p>
                            You cannot directly link to Create Action.
                            GET requests should not have any external side effects, as a user could accidentally trigger it by following a normal link.
                        </p>

                        <h2>Possible Solutions</h2>
                        <p>
                            <a style="text-decoration: none" href="https://ihp.digitallyinduced.com/Guide/form.html" target="_blank">Make a form with <code>formFor</code> to do the request</a>
                        </p>
                    |]
                let title = [hsx|Action was called from a GET request, but needs to be called as a POST request|]
                let RequestContext { respond } = ?context
                respond $ responseBuilder status400 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
            Just Router.UnexpectedMethodException { allowedMethods, method } -> do
                let errorMessage = [hsx|
                        <p>Routing failed with: {tshow exception}</p>
                        <h2>Possible Solutions</h2>
                        <p>
                            <a style="text-decoration: none" href="https://ihp.digitallyinduced.com/Guide/form.html" target="_blank">Make a form with <code>formFor</code> to do the request</a>
                        </p>
                    |]
                let title = [hsx|Action was called with a {method} request, but needs to be called with one of these request methods: <q>{allowedMethods}</q>|]
                let RequestContext { respond } = ?context
                respond $ responseBuilder status400 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
            _ -> do
                let errorMessage = [hsx|
                        Routing failed with: {tshow exception}

                        <h2>Possible Solutions</h2>
                        <p>Are you trying to do a DELETE action, but your link is missing class="js-delete"?</p>
                    |]
                let title = H.text "Routing failed"
                let RequestContext { respond } = ?context
                respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))


renderError :: forall context. (?context :: context, ConfigProvider context) => H.Html -> H.Html -> H.Html
renderError errorTitle view = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>

    <title>IHP Error</title>
    <style>
        * { -webkit-font-smoothing: antialiased }
        h2 {
            color: white;
            font-size: 1.25rem;
        }
        body {
            margin: 0;
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Helvetica Neue", Arial, sans-serif;
        }

        body a {
            color: hsla(196, 13%, 80%, 1);
        }

        .ihp-error-other-solutions {
            margin-top: 2rem;
            padding-top: 0.5rem;
            font-size: 1rem;
            color: hsla(196, 13%, 80%, 1);
            border-top: 1px solid hsla(196, 13%, 60%, 0.4);
        }

        .ihp-error-other-solutions a {
            color: hsla(196, 13%, 80%, 0.9);
            text-decoration: none !important;
            margin-right: 1rem;
            font-size: 0.8rem;
        }
        .ihp-error-other-solutions a:hover {
            color: hsla(196, 13%, 80%, 1);
        }

        .ihp-error-inline-code, .ihp-error-code {
            background-color: rgba(0, 43, 54, 0.5);
            color: white;
            border-radius: 3px;
        }

        .ihp-error-code {
            padding: 1rem;
            overflow-x: auto;
        }

        .ihp-error-inline-code {
            padding: 3px;
            font-family: monospace;
        }
    </style>
</head>
<body>
    <div style="background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1)">
        <div style="max-width: 800px; margin-left: auto; margin-right: auto">
            <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 500; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">{errorTitle}</h1>
            <div style="margin-top: 1rem; font-size: 1.25rem; color:hsla(196, 13%, 80%, 1)">
                {view}
            </div>

            {when shouldShowHelpFooter helpFooter}
        </div>
    </div>
</body>
    |]
        where
            shouldShowHelpFooter = (fromConfig environment) == Environment.Development
            helpFooter = [hsx|
                <div class="ihp-error-other-solutions">
                    <a href="https://stackoverflow.com/questions/tagged/ihp" target="_blank">Ask the IHP Community on StackOverflow</a>
                    <a href="https://github.com/digitallyinduced/ihp/wiki/Troubleshooting" target="_blank">Check the Troubleshooting</a>
                    <a href="https://github.com/digitallyinduced/ihp/issues/new" target="_blank">Open GitHub Issue</a>
                    <a href="https://ihp.digitallyinduced.com/Slack" target="_blank">Slack</a>
                    <a href="https://www.reddit.com/r/IHPFramework/" target="_blank">Reddit</a>
                    <a href="https://stackshare.io/ihp" target="_blank">StackShare</a>
                </div>
            |]
