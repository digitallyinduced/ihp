module TurboHaskell.DevelopmentSupport.LiveReloadNotificationServer where

import ClassyPrelude
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified Network.Wai.Handler.WebSockets as Websocket
import qualified Control.Concurrent as Concurrent
import qualified System.Posix.Signals as Signals

main :: IO ()
main = do
    state <- newMVar []
    Signals.installHandler Signals.keyboardSignal (Signals.Catch (notify state)) Nothing
    Warp.run 8002 $ Websocket.websocketsOr
        Websocket.defaultConnectionOptions
        (app state)
        httpApp

httpApp :: Wai.Application
httpApp request respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

type ClientId = Int
type Client = (ClientId, Websocket.Connection)
type State = [Client]

nextId :: State -> ClientId
nextId = maybe 0 ((+) 1) . maximumMay . map fst

connectClient :: Websocket.Connection -> Concurrent.MVar State -> IO ClientId
connectClient connection stateRef = Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, connection) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

listen :: Websocket.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen connection clientId stateRef = forever $ do
    Websocket.receiveData connection >>= broadcast clientId stateRef

broadcast :: ClientId -> Concurrent.MVar State -> Text -> IO ()
broadcast clientId stateRef message = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    forM_ otherClients $ \(_, connection) -> Websocket.sendTextData connection message

notify :: Concurrent.MVar State -> IO ()
notify stateRef = do
    clients <- Concurrent.readMVar stateRef
    forM_ clients $ \(_, connection) -> Websocket.sendTextData connection ("reload" :: Text)

app :: Concurrent.MVar State -> Websocket.ServerApp
app stateRef pendingConnection = do
    connection <- Websocket.acceptRequest pendingConnection
    clientId <- connectClient connection stateRef
    Websocket.forkPingThread connection 30
    finally
        (listen connection clientId stateRef)
        (disconnectClient clientId stateRef)
