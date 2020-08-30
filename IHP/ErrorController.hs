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
) where

import IHP.Prelude hiding (displayException)
import qualified IHP.Controller.Param as Param
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO (stderr)
import IHP.Controller.RequestContext
import Network.HTTP.Types (status500, status404)
import Network.Wai
import Network.HTTP.Types.Header

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Data.ByteString.Char8 as ByteString

import IHP.HtmlSupport.QQ (hsx)
import Database.PostgreSQL.Simple.FromField (ResultError (..))

handleNoResponseReturned :: (Show controller, ?requestContext :: RequestContext) => controller -> IO ResponseReceived
handleNoResponseReturned controller = do
    let codeSample :: Text = "render MyView { .. }"
    let errorMessage = [hsx|
            <h2>Possible Solutions</h2>
            <p>You can fix this by calling '{codeSample}' at the end of your action.</p>

            <h2>Details</h2>
            <p style="font-size: 16px">No response was returned while running the action {tshow controller}</p>
            
        |]
    let title = [hsx|No response returned in {tshow controller}|]
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

handleNotFound :: (?requestContext :: RequestContext) => IO ResponseReceived
handleNotFound = do
    let errorMessage = [hsx|Router failed to find an action to handle this request.|]
    let title = H.text "Action Not Found"
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseBuilder status404 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

handleRouterException :: (?requestContext :: RequestContext) => SomeException -> IO ResponseReceived
handleRouterException exception = do
    let errorMessage = [hsx|
            Routing failed with: {tshow exception}
            
            <h2>Possible Solutions</h2>
            <p>Are you using AutoRoute but some of your fields are not UUID? In that case <a href="https://ihp.digitallyinduced.com/Guide/routing.html#parameter-types" target="_blank">please see the documentation on Parameter Types</a></p>
        |]
    let title = H.text "Routing failed"
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))


displayException :: (Show action, ?requestContext :: RequestContext) => SomeException -> action -> Text -> IO ResponseReceived
displayException exception action additionalInfo = do
    let allHandlers = [ postgresHandler, paramNotFoundExceptionHandler, patternMatchFailureHandler ]
    let supportingHandlers = allHandlers |> mapMaybe (\f -> f exception action additionalInfo)

    -- Additionally to rendering the error message to the browser we also print out 
    -- the error message to the console because sometimes you cannot easily access the http response
    Text.hPutStrLn stderr (tshow exception)

    supportingHandlers
        |> head
        |> fromMaybe (genericHandler exception action additionalInfo)

genericHandler :: (Show controller, ?requestContext :: RequestContext) => Exception.SomeException -> controller -> Text -> IO ResponseReceived
genericHandler exception controller additionalInfo = do
    let errorMessage = [hsx|An exception was raised while running the action {tshow controller}{additionalInfo}|]
    let (RequestContext _ respond _ _ _) = ?requestContext
    let title = H.string (Exception.displayException exception)
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))

postgresHandler :: (Show controller, ?requestContext :: RequestContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
postgresHandler exception controller additionalInfo = do
    let (RequestContext _ respond _ _ _) = ?requestContext

    let
        handlePostgresError :: Show exception => exception -> Text -> IO ResponseReceived
        handlePostgresError exception errorText =
            let
                title = H.text ("Database looks outdated. " <> errorText)
                errorMessage = [hsx|
                        <h2>Possible Solutions</h2>
                        <div style="margin-bottom: 2rem; font-weight: 400;">
                            Have you clicked on
                            <form method="POST" action="http://localhost:8001/UpdateDb" target="_blank" style="display: inline">
                                <button type="submit">Update DB</button>
                            </form>
                            after updating the Schema?
                        </div>

                        <h2>Details</h2>
                        <p style="font-size: 16px">The exception was raised while running the action: {tshow controller}{additionalInfo}</p>
                        <p style="font-family: monospace; font-size: 16px">{tshow exception}</p>
                    |]
            in
                respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
    case fromException exception of
        Just (exception :: PG.ResultError) -> Just (handlePostgresError exception "The database result does not match the expected type.")
        Nothing -> case fromException exception of
            -- Catching  `relation "..." does not exist`
            Just (exception :: PG.SqlError)
                |  "relation" `ByteString.isPrefixOf` (get #sqlErrorMsg exception)
                && "does not exist" `ByteString.isSuffixOf` (get #sqlErrorMsg exception)
                -> Just (handlePostgresError exception "A table is missing.")
            _ -> Nothing

patternMatchFailureHandler :: (Show controller, ?requestContext :: RequestContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
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
            let (RequestContext _ respond _ _ _) = ?requestContext
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Nothing -> Nothing

-- Handler for 'IHP.Controller.Param.ParamNotFoundException'
paramNotFoundExceptionHandler :: (Show controller, ?requestContext :: RequestContext) => SomeException -> controller -> Text -> Maybe (IO ResponseReceived)
paramNotFoundExceptionHandler exception controller additionalInfo = do
    case fromException exception of
        Just (exception :: Param.ParamNotFoundException) -> Just do
            let (Param.ParamNotFoundException paramName) = exception
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
            let (RequestContext _ respond _ _ _) = ?requestContext
            respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError title errorMessage))
        Nothing -> Nothing

renderError :: _
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
    </style>
</head>
<body>
    <div style="background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1)">
        <div style="max-width: 800px; margin-left: auto; margin-right: auto">
            <h1 style="margin-bottom: 2rem; font-size: 2rem; font-weight: 500; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)">{errorTitle}</h1>
            <div style="margin-top: 1rem; font-size: 1.25rem; color:hsla(196, 13%, 80%, 1)">
                {view}
            </div>

            <div class="ihp-error-other-solutions">
                <a href="https://gitter.im/digitallyinduced/ihp?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge" target="_blank">Ask the IHP Community on Gitter</a>
                <a href="https://github.com/digitallyinduced/ihp/wiki/Troubleshooting" target="_blank">Check the Troubleshooting</a>
                <a href="https://github.com/digitallyinduced/ihp/issues/new" target="_blank">Open a GitHub Issue</a>
            </div>
        </div>
    </div>
</body>
    |]
