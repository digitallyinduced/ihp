module TurboHaskell.ErrorController where

import ClassyPrelude
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import TurboHaskell.Controller.RequestContext
import Data.String.Conversions (cs)
import Network.HTTP.Types (status500)
import Network.Wai
import Network.HTTP.Types.Header

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

handlePatternMatchFailure :: (Show controller, ?requestContext :: RequestContext) => Exception.PatternMatchFail -> controller -> IO ResponseReceived
handlePatternMatchFailure exception controller = do
    let (controllerPath, _) = Text.breakOn ":" (tshow exception)
    let errorMessage = ("Missing action case for " <> tshow controller <> ".\n\nYou can fix this by adding an action handler like this to the controller '" <> controllerPath <> "':\n\n    action (" <> tshow controller <> ") = do\n        renderPlain \"Hello World\"\n\nGHC Exception: " <> tshow exception)
    putStr errorMessage
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError "Missing action" errorMessage))



handleNoResponseReturned :: (Show controller, ?requestContext :: RequestContext) => controller -> IO ResponseReceived
handleNoResponseReturned controller = do
    let errorMessage = ("No response was returned while running the action " <> tshow controller <> ".\n\nYou can fix this by calling 'render MyView { .. }' at the end of your action.")
    putStr errorMessage
    let (RequestContext _ respond _ _ _) = ?requestContext
    respond $ responseBuilder status500 [(hContentType, "text/html")] (Blaze.renderHtmlBuilder (renderError "No response returned" errorMessage))




renderError :: _
renderError errorTitle view = H.docTypeHtml ! A.lang "en" $ do
    H.head do
        H.meta ! A.charset "utf-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
        H.title "Error"
    H.body ! A.style "margin: 0; font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", \"Roboto\", \"Helvetica Neue\", Arial, sans-serif;" $ do
        H.div ! A.style "background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1)" $ do
            H.div ! A.style "max-width: 800px; margin-left: auto; margin-right: auto" $ do
                H.h1 ! A.style "margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)" $ errorTitle
                H.pre ! A.style "margin-top: 1rem; font-size: 1.25rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)" $ H.text view