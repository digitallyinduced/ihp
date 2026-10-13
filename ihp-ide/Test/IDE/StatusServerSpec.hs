module IDE.StatusServerSpec where

import IHP.Prelude
import Test.Hspec
import IHP.IDE.StatusServer
import IHP.IDE.Types
import IHP.IDE.PortConfig
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import Control.Monad (replicateM_)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Concurrent.Chan.Unagi as Queue
import qualified Network.Socket as Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as Websocket
import qualified System.Timeout as Timeout

tests :: Spec
tests = describe "status server handover" do
    it "closes existing WebSockets on each handover to the app" do
        result <- Timeout.timeout 10000000 $ withStatusServerFixture \port startServer stopServer _ -> do
            replicateM_ 2 do
                Websocket.runClient "127.0.0.1" port "/" \firstConnection ->
                  Websocket.runClient "127.0.0.1" port "/" \secondConnection -> do
                    stopped <- MVar.newEmptyMVar
                    MVar.putMVar stopServer stopped
                    MVar.takeMVar stopped
                    expectClosed firstConnection
                    expectClosed secondConnection
                MVar.putMVar startServer ()
        result `shouldBe` Just ()

    it "closes existing WebSockets when the enclosing server is cancelled" do
        result <- Timeout.timeout 10000000 $ withStatusServerFixture \port _ _ cancelServer ->
            Websocket.runClient "127.0.0.1" port "/" \connection -> do
                cancelServer
                expectClosed connection
        result `shouldBe` Just ()

expectClosed :: Websocket.Connection -> Expectation
expectClosed connection = do
    closed <- Timeout.timeout 2000000
        (Exception.try @Websocket.ConnectionException (Websocket.receiveData @Text connection))
    closed `shouldSatisfy` \case
        Just (Left Websocket.ConnectionClosed) -> True
        Just (Left (Websocket.CloseRequest _ _)) -> True
        _ -> False

withStatusServerFixture :: (Int -> MVar () -> MVar (MVar ()) -> IO () -> IO ()) -> IO ()
withStatusServerFixture test =
    Exception.bracket Warp.openFreePort (Socket.close . snd) \(port, appSocket) -> do
        (ghciInChan, ghciOutChan) <- Queue.newChan
        liveReloadClients <- newIORef mempty
        lastSchemaCompilerError <- newIORef Nothing
        loading <- newIORef False
        controls <- MVar.newEmptyMVar
        finished <- MVar.newEmptyMVar
        let ?context = Context
                { portConfig = PortConfig (fromIntegral port) 0
                , isDebugMode = False
                , logger = const (pure ())
                , ghciInChan
                , ghciOutChan
                , liveReloadClients
                , wrapWithDirenv = False
                , lastSchemaCompilerError
                , appSocket
                }
        Async.withAsync (withStatusServer loading \startServer stopServer _ _ _ -> do
            MVar.putMVar controls (startServer, stopServer)
            MVar.takeMVar finished) \server -> do
                Async.link server
                (startServer, stopServer) <- MVar.takeMVar controls
                test port startServer stopServer (Async.cancel server)
