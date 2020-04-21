module TurboHaskell.IDE.ToolServer where

import TurboHaskell.Prelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import TurboHaskell.HaskellSupport
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Network.HTTP.Types.Header as HTTP
import qualified Text.Blaze.Html5 as Html5
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString
import TurboHaskell.IDE.Types
import TurboHaskell.IDE.PortConfig
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import qualified TurboHaskell.ErrorController as ErrorController
import TurboHaskell.ApplicationContext
import TurboHaskell.ModelSupport
import TurboHaskell.RouterSupport hiding (get)
import qualified Web.Cookie as Cookie
import qualified Data.Time.Clock
import Network.Wai.Session.ClientSession (clientsessionStore)
import Network.Wai.Session.Map (mapStore_)
import qualified Web.ClientSession as ClientSession
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Network.Wai.Middleware.Static
import Unsafe.Coerce
import Network.Wai.Session (withSession, Session)
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.SchemaDesigner.Controller
import TurboHaskell.IDE.ToolServer.Types
import Control.Concurrent.Async
import TurboHaskell.IDE.ToolServer.Routes
import qualified System.Process as Process


startToolServer :: (?context :: Context) => IO ()
startToolServer = do
    let port = ?context
            |> get #portConfig
            |> get #toolServerPort
            |> fromIntegral



    thread <- async (startToolServer' port)

    --openUrl ("http://localhost:" <> tshow port <> "/turbohaskell/")
    putStrLn "ToolServer started"

    dispatch (UpdateToolServerState (ToolServerStarted { thread }))
    
startToolServer' port = do
    session <- Vault.newKey
    --store <- fmap clientsessionStore (ClientSession.getKey "Config/client_session_key.aes")
    store <- mapStore_
    let sessionCookie = def
                { Cookie.setCookiePath = Just "/"
                , Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 30))
                , Cookie.setCookieSameSite = Just Cookie.sameSiteLax
                }
    let sessionMiddleware :: Wai.Middleware = withSession store "SESSION" sessionCookie session    
    let applicationContext = ApplicationContext { modelContext = (ModelContext (error "Not connected")), session }
    let application :: Wai.Application = \request respond -> do
            let ?applicationContext = applicationContext
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            frontControllerToWAIApp @ToolServerApplication ErrorController.handleNotFound
            
    let staticMiddleware :: Wai.Middleware = staticPolicy (addBase "TurboHaskell/TurboHaskell/static/")

    let warpSettings = Warp.defaultSettings |> Warp.setPort 8002
    
    Warp.runSettings warpSettings $ 
            staticMiddleware $ logStdoutDev $ methodOverridePost $ sessionMiddleware $ application

stopToolServer ToolServerStarted { thread } = uninterruptibleCancel thread
stopToolServer ToolServerNotStarted = pure ()

openUrl :: Text -> IO ()
openUrl url = do
    Process.callCommand (cs $ "open " <> url)
    pure ()

instance FrontController ToolServerApplication where
    controllers =
        [ parseRoute @SchemaDesignerController
        , catchAll TablesAction
        ]

instance ControllerSupport.InitControllerContext ToolServerApplication where