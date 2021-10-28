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
defaultNotFoundResponse = do
    let errorMessage = [hsx|Router failed to find an action to handle this request.|]
    let title = H.text "Action Not Found"
    responseBuilder status404 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

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
        handlePostgresError :: Show exception => exception -> Text -> IO ResponseReceived
        handlePostgresError exception errorText = do
            let ihpIdeBaseUrl = ?context
                    |> getFrameworkConfig
                    |> get #ideBaseUrl
            let title = H.text ("Database looks outdated. " <> errorText)
            let errorMessage = [hsx|
                        <h2>Possible Solutions</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            Have you clicked on
                            <form method="POST" action={ihpIdeBaseUrl <> "/UpdateDb"} target="_blank" style="display: inline">
                                <button type="submit">Update DB</button>
                            </form>
                            after updating the Schema?
                        </div>

                        <h2>Details</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            let RequestContext { respond } = get #requestContext ?context
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
    case fromException exception of
        Just (exception :: PG.ResultError) -> Just (handlePostgresError exception "The database result does not match the expected type.")
        Nothing -> case fromException exception of
            -- Catching  `relation "..." does not exist`
            Just (exception :: PG.SqlError)
                |  "relation" `ByteString.isPrefixOf` (get #sqlErrorMsg exception)
                && "does not exist" `ByteString.isSuffixOf` (get #sqlErrorMsg exception)
                -> Just (handlePostgresError exception "A table is missing.")
            Just (exception :: PG.SqlError)
                |  "column" `ByteString.isPrefixOf` (get #sqlErrorMsg exception)
                && "does not exist" `ByteString.isSuffixOf` (get #sqlErrorMsg exception)
                -> Just (handlePostgresError exception "A column is missing.")
            _ -> Nothing

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
                    <a href="https://gitter.im/digitallyinduced/ihp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge" target="_blank">Gitter</a>
                    <a href="https://www.reddit.com/r/IHPFramework/" target="_blank">Reddit</a>
                    <a href="https://stackshare.io/ihp" target="_blank">StackShare</a>
                </div>
            |]
