{-|
Module: IHP.ErrorController
Description:  Provides web-based error screens for runtime errors in IHP
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ErrorController
( displayException
, errorHandlerMiddleware
, RouterException(..)
, InitContextException(..)
) where

import Prelude
import Control.Exception.Safe (SomeException, fromException, catch)
import Control.Monad (when)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import IHP.HaskellSupport (isEmpty, forEach, (|>))
import qualified IHP.Controller.Param as Param
import qualified IHP.Router.Types as Router
import qualified Network.HTTP.Types.Method as Router
import qualified Control.Exception as Exception
import Data.Text (Text)
import Wai.Request.Params.Middleware (Respond)
import Network.HTTP.Types (Status, status404, status500, status400)
import Network.Wai (Request, Middleware, Response, ResponseReceived, responseBuilder, responseLBS, queryString, requestHeaders, vault)
import Network.HTTP.Types.Header
import qualified Network.HTTP.Media as Accept
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

import IHP.HSX.Markup (Markup, MarkupM(..), ToHtml(..), getBuilder)
import qualified Database.PostgreSQL.Simple as PG
import qualified Hasql.Errors as HasqlErrors
import qualified Hasql.Pool as HasqlPool

import IHP.HSX.MarkupQQ (hsx)
import qualified IHP.ModelSupport as ModelSupport
import IHP.FrameworkConfig
import qualified IHP.Environment as Environment
import IHP.Controller.Context
import IHP.Controller.NotFound (handleNotFound, buildNotFoundResponse)
import qualified IHP.Log as Log
import IHP.Log (writeLog)
import IHP.Log.Types (LogLevel(..))
import IHP.ActionType (actionTypeVaultKey)
import qualified Data.Vault.Lazy as Vault

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Wrapper for exceptions that occur during routing.
-- This allows the error handler middleware to distinguish routing errors
-- from action errors and display appropriate error messages.
newtype RouterException = RouterException SomeException
    deriving (Show)

instance Exception.Exception RouterException

-- | Wrapper for exceptions that occur during 'initContext'.
-- This allows the error handler middleware to show "while calling initContext"
-- in the error message, helping developers locate the source of the problem.
newtype InitContextException = InitContextException SomeException
    deriving (Show)

instance Exception.Exception InitContextException

-- | Returns 'True' only when the request's Accept header explicitly prefers
-- @application/json@ over @text/html@. @Accept: */*@ or a missing header
-- stay on the HTML path (the browser-friendly default).
--
-- The server options are listed with @text/html@ first so that ties
-- (equal quality and specificity) resolve to HTML — this covers
-- @Accept: */*@ (curl's default, fetch() without custom headers) and
-- @Accept: text/html, application/json@ (typical browser ordering).
-- JSON wins only when the client excludes HTML or gives it a lower
-- q-value.
wantsJsonResponse :: Request -> Bool
wantsJsonResponse request =
    case lookup hAccept (requestHeaders request) of
        Nothing -> False
        Just accept ->
            Accept.mapAcceptMedia
                [ ("text/html", False)
                , ("application/json", True)
                ] accept == Just True

-- | Render an error response, picking HTML or JSON based on the Accept header.
--
-- Callers provide both the HTML markup (title + body, rendered via 'renderError')
-- and a JSON payload. The JSON path uses 'responseLBS' with @application/json@;
-- the HTML path mirrors the existing 'responseBuilder'/'renderError' pipeline.
respondError
    :: Request
    -> Environment.Environment
    -> Status
    -> Markup
    -> Markup
    -> Aeson.Value
    -> IO Response
respondError request environment status title body json
  | wantsJsonResponse request =
        pure $ responseLBS status
            [(hContentType, "application/json")]
            (Aeson.encode json)
  | otherwise =
        pure $ responseBuilder status
            [(hContentType, "text/html")]
            (getBuilder (renderError environment title body))

displayException :: (Show action, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => SomeException -> action -> Text -> IO ResponseReceived
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

    let allHandlers = if ?context.frameworkConfig.environment == Environment.Development
            then devHandlers
            else prodHandlers

    let supportingHandlers = allHandlers |> mapMaybe (\f -> f exception action additionalInfo)

    let displayGenericError = genericHandler exception action additionalInfo


    -- Additionally to rendering the error message to the browser we also send it
    -- to the error tracking service (e.g. sentry). Usually this service also writes
    -- the error message to the stderr output
    --
    when (?context.frameworkConfig.environment == Environment.Production) do
        let exceptionTracker = ?context.frameworkConfig.exceptionTracker.onException
        exceptionTracker (Just ?request) exception

    supportingHandlers
        |> listToMaybe
        |> fromMaybe displayGenericError

-- | Responds to all exceptions with a generic error message.
--
-- In dev mode the action and exception is added to the output.
-- In production mode nothing is specific is communicated about the exception
genericHandler :: (Show controller, ?context :: ControllerContext, ?respond :: Respond) => Exception.SomeException -> controller -> Text -> IO ResponseReceived
genericHandler exception controller additionalInfo = do
    let errorMessageText = "An exception was raised while running the action " <> tshow controller <> additionalInfo
    let errorMessageTitle = Exception.displayException exception

    let devErrorMessage = [hsx|{errorMessageText}|]
    let devTitle = [hsx|{errorMessageTitle}|]

    Log.error (errorMessageText <> ": " <> cs errorMessageTitle)

    let prodErrorMessage = [hsx|An exception was raised while running the action|]
    let prodTitle = [hsx|An error happened|]

    let (errorMessage, errorTitle) = if ?context.frameworkConfig.environment == Environment.Development
            then (devErrorMessage, devTitle)
            else (prodErrorMessage, prodTitle)

    ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError ?context.frameworkConfig.environment errorTitle errorMessage) |> getBuilder)

postgresHandler :: (Show controller, ?context :: ControllerContext, ?respond :: Respond) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
postgresHandler exception controller additionalInfo = do
    let
        handlePostgresOutdatedError :: Text -> Markup -> IO ResponseReceived
        handlePostgresOutdatedError errorDetail errorText = do
            let ihpIdeBaseUrl = ?context.frameworkConfig.ideBaseUrl
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
                        <p style="font-family: monospace; font-size: 16px">{errorDetail}</p>
                    |]
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)

        handleServerError :: Text -> [Text] -> HasqlErrors.ServerError -> IO ResponseReceived
        handleServerError sql params (HasqlErrors.ServerError code msg detail hint _position) = do
            let title = [hsx|{msg}|]
            let detailSection = case detail of
                    Just d  -> [hsx|<p style="font-size: 16px"><strong>Detail:</strong> {d}</p>|]
                    Nothing -> mempty
            let hintSection = case hint of
                    Just h  -> [hsx|<p style="font-size: 16px"><strong>Hint:</strong> {h}</p>|]
                    Nothing -> mempty
            let paramsText = Text.intercalate ", " params
            let errorMessage = [hsx|
                        <h2>While running the following query:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <pre class="ihp-error-code">{sql}</pre>
                        </div>

                        <h2>With parameters:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <code>{paramsText}</code>
                        </div>

                        {detailSection}
                        {hintSection}

                        <p style="font-size: 14px; font-family: monospace;">PostgreSQL error code: {code}</p>

                        <h2>Details:</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                    |]
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)

        handleSessionError :: HasqlErrors.SessionError -> IO ResponseReceived
        handleSessionError sessionError = do
            let title = [hsx|PostgreSQL Error|]
            let errorMessage = [hsx|
                        <h2>Details:</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <pre class="ihp-error-code">{tshow sessionError}</pre>
                    |]
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)

    case fromException exception of
        Just (ModelSupport.HasqlError (HasqlPool.SessionUsageError sessionError)) -> Just case sessionError of
            -- Statement with a ServerError
            HasqlErrors.StatementSessionError _pipelineSize _stmtIdx sql params _prepared (HasqlErrors.ServerStatementError serverError@(HasqlErrors.ServerError code _msg _ _ _))
                -- 42P01 = undefined_table ("relation ... does not exist")
                | code == "42P01" -> handlePostgresOutdatedError (tshow serverError) "A table is missing."
                -- 42703 = undefined_column ("column ... does not exist")
                | code == "42703" -> handlePostgresOutdatedError (tshow serverError) "A column is missing."
                -- All other server errors on statements
                | otherwise -> handleServerError sql params serverError
            -- Script (multi-statement) with a ServerError
            HasqlErrors.ScriptSessionError sql serverError@(HasqlErrors.ServerError code _msg _ _ _)
                | code == "42P01" -> handlePostgresOutdatedError (tshow serverError) "A table is missing."
                | code == "42703" -> handlePostgresOutdatedError (tshow serverError) "A column is missing."
                | otherwise -> handleServerError sql [] serverError
            -- Any other session error (connection errors, type mismatches, etc.)
            other -> handleSessionError other
        Just (ModelSupport.HasqlError _otherUsageError) -> Just do
            let title = [hsx|Database Connection Error|]
            let errorMessage = [hsx|
                        <h2>Details:</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <pre class="ihp-error-code">{tshow _otherUsageError}</pre>
                    |]
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)
        Nothing -> Nothing

patternMatchFailureHandler :: (Show controller, ?context :: ControllerContext, ?respond :: Respond) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
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
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)
        Nothing -> Nothing

-- Handler for 'IHP.Controller.Param.ParamNotFoundException'
-- Only used in dev mode of the app.

paramNotFoundExceptionHandler :: (Show controller, ?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
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
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)
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
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)
        Nothing -> Nothing

-- Handler for 'IHP.ModelSupport.RecordNotFoundException'
--
-- Used only in development mode of the app.
recordNotFoundExceptionHandlerDev :: (Show controller, ?context :: ControllerContext, ?respond :: Respond) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
recordNotFoundExceptionHandlerDev exception controller additionalInfo =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException { queryAndParams })) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)
            let errorMessage = [hsx|
                    <p>
                        The following SQL was executed:
                        <pre class="ihp-error-code">{queryAndParams}</pre>
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
            ?respond $ responseBuilder status500 [(hContentType, "text/html")] ((renderError Environment.Development title errorMessage) |> getBuilder)
        Nothing -> Nothing

-- Handler for 'IHP.ModelSupport.RecordNotFoundException'
--
-- Used only in production mode of the app. The exception is handled by calling 'handleNotFound'
recordNotFoundExceptionHandlerProd :: (?context :: ControllerContext, ?request :: Request, ?respond :: Respond) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
recordNotFoundExceptionHandlerProd exception controller additionalInfo =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException {})) ->
            Just (handleNotFound ?request ?respond)
        Nothing -> Nothing

renderError :: Environment.Environment -> Markup -> Markup -> Markup
renderError environment errorTitle view = [hsx|
<!DOCTYPE html>
<html lang="en">
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
</html>
    |]
        where
            shouldShowHelpFooter = environment == Environment.Development
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

-- | Middleware that catches exceptions and displays appropriate error pages.
--
-- This middleware should be placed near the top of the middleware stack so it can
-- catch exceptions from controllers, routing, and other middleware.
errorHandlerMiddleware :: FrameworkConfig -> Middleware
errorHandlerMiddleware frameworkConfig app request respond =
    app request respond `catch` \(exception :: SomeException) -> do
        let environment = frameworkConfig.environment
        let actionType = Vault.lookup actionTypeVaultKey (vault request)
        let actionDescription = maybe "" (\(ActionType t) -> " while running " <> tshow t) actionType

        -- Unwrap InitContextException to get the inner exception and add context
        let (actualException, fullDescription) = case fromException exception of
                Just (InitContextException inner) -> (inner, actionDescription <> " while calling initContext")
                Nothing -> (exception, actionDescription)

        -- Call exception tracker in production
        when (environment == Environment.Production) do
            frameworkConfig.exceptionTracker.onException (Just request) actualException

        response <- handleExceptionMiddleware frameworkConfig request actualException fullDescription
        respond response

-- | Handle an exception and return an appropriate Response.
--
-- This is used by the error handler middleware.
handleExceptionMiddleware :: FrameworkConfig -> Request -> SomeException -> Text -> IO Response
handleExceptionMiddleware frameworkConfig request exception actionDescription = do
    let environment = frameworkConfig.environment

    -- Dev handlers display helpful tips on how to resolve the problem
    let devHandlers =
            [ routerExceptionHandlerMiddleware frameworkConfig request
            , postgresHandlerMiddleware frameworkConfig request actionDescription
            , paramNotFoundExceptionHandlerMiddleware frameworkConfig request actionDescription
            , patternMatchFailureHandlerMiddleware frameworkConfig request actionDescription
            , recordNotFoundExceptionHandlerDevMiddleware frameworkConfig request actionDescription
            ]

    -- Prod handlers should not leak any information about the system
    let prodHandlers =
            [ routerExceptionHandlerMiddleware frameworkConfig request
            , recordNotFoundExceptionHandlerProdMiddleware frameworkConfig request
            ]

    let allHandlers = if environment == Environment.Development
            then devHandlers
            else prodHandlers

    let supportingHandlers = allHandlers |> mapMaybe (\f -> f exception)

    let displayGenericError = genericHandlerMiddleware frameworkConfig request exception actionDescription

    supportingHandlers
        |> listToMaybe
        |> fromMaybe displayGenericError

-- | Generic error handler for middleware - returns a Response
genericHandlerMiddleware :: FrameworkConfig -> Request -> Exception.SomeException -> Text -> IO Response
genericHandlerMiddleware frameworkConfig request exception actionDescription = do
    let environment = frameworkConfig.environment
    let errorMessageText = "An exception was raised" <> actionDescription
    let errorMessageTitle = Exception.displayException exception

    let devErrorMessage = [hsx|{errorMessageText}|]
    let devTitle = [hsx|{errorMessageTitle}|]

    writeLog Error frameworkConfig.logger (errorMessageText <> ": " <> cs errorMessageTitle)

    let prodErrorMessage = [hsx|An exception was raised while running the action|]
    let prodTitle = [hsx|An error happened|]

    let (errorMessage, errorTitle, jsonPayload) = if environment == Environment.Development
            then
                ( devErrorMessage
                , devTitle
                , Aeson.object
                    [ "error" .= Text.pack errorMessageTitle
                    , "message" .= errorMessageText
                    ]
                )
            else
                ( prodErrorMessage
                , prodTitle
                , Aeson.object
                    [ "error" .= ("An error happened" :: Text)
                    , "message" .= ("An exception was raised while running the action" :: Text)
                    ]
                )

    respondError request environment status500 errorTitle errorMessage jsonPayload

-- | Postgres error handler for middleware - returns Maybe (IO Response)
postgresHandlerMiddleware :: FrameworkConfig -> Request -> Text -> SomeException -> Maybe (IO Response)
postgresHandlerMiddleware frameworkConfig request actionDescription exception = do
    let
        handlePostgresOutdatedError :: Show exception => exception -> Text -> IO Response
        handlePostgresOutdatedError exception errorText = do
            let ihpIdeBaseUrl = frameworkConfig.ideBaseUrl
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
                        <p style="font-size: 16px">The exception was raised{actionDescription}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            let json = Aeson.object
                    [ "error" .= ("Database looks outdated" :: Text)
                    , "reason" .= errorText
                    , "message" .= ("The exception was raised" <> actionDescription)
                    , "detail" .= tshow exception
                    ]
            respondError request Environment.Development status500 title errorMessage json

        handleSqlError :: ModelSupport.EnhancedSqlError -> IO Response
        handleSqlError exception = do
            let sqlError = exception.sqlError
            let title = [hsx|{sqlError.sqlErrorMsg}|]
            let errorMessage = [hsx|
                        <h2>While running the following Query:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <code>{exception.sqlErrorQuery}</code>
                        </div>

                        <h2>With Query Parameters:</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            <code>{exception.sqlErrorQueryParams}</code>
                        </div>

                        <h2>Details:</h2>
                        <p style="font-size: 16px">The exception was raised{actionDescription}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            let json = Aeson.object
                    [ "error" .= Text.decodeUtf8Lenient sqlError.sqlErrorMsg
                    , "query" .= tshow exception.sqlErrorQuery
                    , "params" .= exception.sqlErrorQueryParams
                    , "message" .= ("The exception was raised" <> actionDescription)
                    , "detail" .= tshow exception
                    ]
            respondError request Environment.Development status500 title errorMessage json

    case fromException exception of
        Just (exception :: PG.ResultError) -> Just (handlePostgresOutdatedError exception "The database result does not match the expected type.")
        Nothing -> case fromException exception of
            -- Catching  `relation "..." does not exist`
            Just exception@ModelSupport.EnhancedSqlError { sqlError }
                |  "relation" `ByteString.isPrefixOf` (sqlError.sqlErrorMsg)
                && "does not exist" `ByteString.isSuffixOf` (sqlError.sqlErrorMsg)
                -> Just (handlePostgresOutdatedError exception "A table is missing.")

            -- Catching  `columns "..." does not exist`
            Just exception@ModelSupport.EnhancedSqlError { sqlError }
                |  "column" `ByteString.isPrefixOf` (sqlError.sqlErrorMsg)
                && "does not exist" `ByteString.isSuffixOf` (sqlError.sqlErrorMsg)
                -> Just (handlePostgresOutdatedError exception "A column is missing.")
            -- Catching other SQL Errors
            Just exception -> Just (handleSqlError exception)
            Nothing -> Nothing

-- | Pattern match failure handler for middleware - returns Maybe (IO Response)
patternMatchFailureHandlerMiddleware :: FrameworkConfig -> Request -> Text -> SomeException -> Maybe (IO Response)
patternMatchFailureHandlerMiddleware frameworkConfig request actionDescription exception = do
    case fromException exception of
        Just (exception :: Exception.PatternMatchFail) -> Just do
            let (controllerPath, _) = Text.breakOn ":" (tshow exception)
            let errorMessage = [hsx|
                    <h2>Possible Solutions</h2>
                    <p>a) Maybe the action function is missing? You can fix this by adding an action handler like this to the controller '{controllerPath}':</p>
                    <pre>{codeSample}</pre>
                    <p style="margin-bottom: 2rem">b) A pattern match like 'let (Just value) = ...' failed. Please see the details section.</p>

                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]
                    where
                        codeSample = "    action (MyAction) = do\n        renderPlain \"Hello World\"" :: Text

            let title = [hsx|Pattern match failed{actionDescription}|]
            let json = Aeson.object
                    [ "error" .= ("Pattern match failed" :: Text)
                    , "message" .= ("Pattern match failed" <> actionDescription)
                    , "details" .= tshow exception
                    , "controllerPath" .= controllerPath
                    ]
            respondError request Environment.Development status500 title errorMessage json
        Nothing -> Nothing

-- | Param not found handler for middleware - returns Maybe (IO Response)
paramNotFoundExceptionHandlerMiddleware :: FrameworkConfig -> Request -> Text -> SomeException -> Maybe (IO Response)
paramNotFoundExceptionHandlerMiddleware frameworkConfig request actionDescription exception = do
    let allParams = queryString request
    let renderParam (paramName, paramValue) = [hsx|<li>{paramName}: {fromMaybe "" paramValue}</li>|]
    case fromException exception of
        Just (exception@(Param.ParamNotFoundException paramName)) -> Just do
            let solutionHint =
                    if isEmpty allParams
                        then [hsx|
                                This action was called without any parameters at all.
                                You can pass this parameter by appending <code>?{paramName}=someValue</code> to the URL.
                            |]
                        else [hsx|
                            <p>The following parameters are provided by the request:</p>
                            <ul>{forEach allParams renderParam}</ul>

                            <p>a) Is there a typo in your call to <code>param {tshow paramName}</code>?</p>
                            <p>b) You can pass this parameter by appending <code>&{paramName}=someValue</code> to the URL.</p>
                            <p>c) You can pass this parameter using a form input like <code>{"<input type=\"text\" name=\"" <> paramName <> "\"/>" :: ByteString}</code>.</p>
                        |]
            let errorMessage = [hsx|
                    <h2>
                        This exception was caused by a call to <code>param {tshow paramName}</code>{actionDescription}.
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
            let availableParams = map (\(n, _) -> Text.decodeUtf8Lenient n) allParams
            let json = Aeson.object
                    [ "error" .= ("Parameter not found" :: Text)
                    , "param" .= Text.decodeUtf8Lenient paramName
                    , "availableParams" .= availableParams
                    , "message" .= ("The exception was raised by a call to param " <> tshow paramName <> actionDescription)
                    ]
            respondError request Environment.Development status500 title errorMessage json
        Just (exception@(Param.ParamCouldNotBeParsedException { name, parserError })) -> Just do
            let errorMessage = [hsx|
                    <h2>
                        This exception was caused by a call to <code>param {tshow name}</code>{actionDescription}.
                    </h2>
                    <p>
                        Here's the error output from the parser: {parserError}
                    </p>

                    <h2>Details</h2>
                    <p style="font-size: 16px">{exception}</p>
                |]

            let title = [hsx|Parameter <q>{name}</q> was invalid|]
            let json = Aeson.object
                    [ "error" .= ("Parameter invalid" :: Text)
                    , "param" .= Text.decodeUtf8Lenient name
                    , "parserError" .= Text.decodeUtf8Lenient parserError
                    , "message" .= ("The exception was raised by a call to param " <> tshow name <> actionDescription)
                    ]
            respondError request Environment.Development status500 title errorMessage json
        Nothing -> Nothing

-- | Record not found handler for middleware (dev mode) - returns Maybe (IO Response)
recordNotFoundExceptionHandlerDevMiddleware :: FrameworkConfig -> Request -> Text -> SomeException -> Maybe (IO Response)
recordNotFoundExceptionHandlerDevMiddleware frameworkConfig request actionDescription exception =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException { queryAndParams })) -> Just do
            let errorMessage = [hsx|
                    <p>
                        The following SQL was executed:
                        <pre class="ihp-error-code">{queryAndParams}</pre>
                    </p>

                    <p>
                        This exception was caused by a call to <code>fetchOne</code>{actionDescription}.
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
            let json = Aeson.object
                    [ "error" .= ("Call to fetchOne failed. No records returned." :: Text)
                    , "queryAndParams" .= queryAndParams
                    , "message" .= ("The exception was raised by a call to fetchOne" <> actionDescription)
                    ]
            respondError request Environment.Development status500 title errorMessage json
        Nothing -> Nothing

-- | Record not found handler for middleware (prod mode) - returns Maybe (IO Response)
recordNotFoundExceptionHandlerProdMiddleware :: FrameworkConfig -> Request -> SomeException -> Maybe (IO Response)
recordNotFoundExceptionHandlerProdMiddleware frameworkConfig request exception =
    case fromException exception of
        Just (exception@(ModelSupport.RecordNotFoundException {})) -> Just $
            if wantsJsonResponse request
                then pure $ responseLBS status404
                        [(hContentType, "application/json")]
                        (Aeson.encode (Aeson.object [ "error" .= ("Not found" :: Text) ]))
                else buildNotFoundResponse
        Nothing -> Nothing

-- | Router exception handler for middleware - returns Maybe (IO Response)
--
-- Handles exceptions thrown during routing. These are wrapped in RouterException
-- by frontControllerToWAIApp to distinguish them from action exceptions.
routerExceptionHandlerMiddleware :: FrameworkConfig -> Request -> SomeException -> Maybe (IO Response)
routerExceptionHandlerMiddleware frameworkConfig request exception =
    let environment = frameworkConfig.environment
    in case fromException exception of
        Just (RouterException innerException) ->
            -- This is a router exception - handle specific types or show generic "Routing failed"
            Just $ handleRouterExceptionImpl request environment innerException
        Nothing ->
            -- Not a router exception
            Nothing

-- | Implementation for handling unwrapped router exceptions
handleRouterExceptionImpl :: Request -> Environment.Environment -> SomeException -> IO Response
handleRouterExceptionImpl request environment exception =
    case fromException exception of
        Just Router.NoConstructorMatched { expectedType, value, field } -> do
            let routingError = if environment == Environment.Development
                then [hsx|<p>Routing failed with: {tshow exception}</p>|]
                else ""

            let errorMessage = [hsx|
                    { routingError }

                    <h2>Possible Solutions</h2>
                    <p>You can pass this parameter by appending <code>&{field}=someValue</code> to the URL.</p>
                |]

            let title = case value of
                    Just value -> [hsx|Expected <strong>{expectedType}</strong> for field <strong>{field}</strong> but got <q>{value}</q>|]
                    Nothing -> [hsx|The action was called without the required <q>{field}</q> parameter|]
            let json = Aeson.object
                    [ "error" .= ("Routing failed" :: Text)
                    , "expectedType" .= Text.decodeUtf8Lenient expectedType
                    , "field" .= Text.decodeUtf8Lenient field
                    , "value" .= fmap Text.decodeUtf8Lenient value
                    ]
            respondError request environment status400 title errorMessage json
        Just Router.BadType { expectedType, value = Just value, field } -> do
            let errorMessage = [hsx|
                    <p>Routing failed with: {tshow exception}</p>
                |]
            let title = [hsx|Query parameter <q>{field}</q> needs to be a <q>{expectedType}</q> but got <q>{value}</q>|]
            let json = Aeson.object
                    [ "error" .= ("Routing failed" :: Text)
                    , "field" .= Text.decodeUtf8Lenient field
                    , "expectedType" .= Text.decodeUtf8Lenient expectedType
                    , "value" .= Text.decodeUtf8Lenient value
                    ]
            respondError request environment status400 title errorMessage json
        _ -> case fromException exception of
            Just Router.UnexpectedMethodException { allowedMethods = [Router.DELETE], method = Router.GET } -> do
                let exampleLink :: Text = "<a href={DeleteProjectAction} class=\"js-delete\">Delete Project</a>"
                let formExample :: Text = "<form method=\"POST\" action={DeleteProjectAction}>\n    <input type=\"hidden\" name=\"_method\" value=\"DELETE\"/>\n    <button type=\"submit\">Delete Project</button>\n</form>"
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
                let json = Aeson.object
                        [ "error" .= ("Unexpected HTTP method" :: Text)
                        , "method" .= tshow Router.GET
                        , "allowedMethods" .= [tshow Router.DELETE]
                        ]
                respondError request environment status400 title errorMessage json
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
                let json = Aeson.object
                        [ "error" .= ("Unexpected HTTP method" :: Text)
                        , "method" .= tshow Router.GET
                        , "allowedMethods" .= [tshow Router.POST]
                        ]
                respondError request environment status400 title errorMessage json
            Just Router.UnexpectedMethodException { allowedMethods, method } -> do
                let errorMessage = [hsx|
                        <p>Routing failed with: {tshow exception}</p>
                        <h2>Possible Solutions</h2>
                        <p>
                            <a style="text-decoration: none" href="https://ihp.digitallyinduced.com/Guide/form.html" target="_blank">Make a form with <code>formFor</code> to do the request</a>
                        </p>
                    |]
                let title = [hsx|Action was called with a {method} request, but needs to be called with one of these request methods: <q>{allowedMethods}</q>|]
                let json = Aeson.object
                        [ "error" .= ("Unexpected HTTP method" :: Text)
                        , "method" .= tshow method
                        , "allowedMethods" .= map tshow allowedMethods
                        ]
                respondError request environment status400 title errorMessage json
            -- Fallback for any other exception during routing
            _ -> do
                let errorMessage = [hsx|
                        Routing failed with: {tshow exception}

                        <h2>Possible Solutions</h2>
                        <p>Are you trying to do a DELETE action, but your link is missing class="js-delete"?</p>
                    |]
                let title = toHtml ("Routing failed" :: Text)
                let json = Aeson.object
                        [ "error" .= ("Routing failed" :: Text)
                        , "detail" .= tshow exception
                        ]
                respondError request environment status500 title errorMessage json
