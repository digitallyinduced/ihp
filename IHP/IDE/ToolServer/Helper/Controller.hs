{-|
Module: IHP.IDE.ToolServer.Helper.Controller
Description: Provides helpers for controllers of the ToolServer
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.ToolServer.Helper.Controller
( appPort
, openEditor
, findWebControllers
, findControllers
, findApplications
, theDevServerContext
, clearDatabaseNeedsMigration
, markDatabaseNeedsMigration
) where

import IHP.Prelude
import IHP.ControllerSupport
import IHP.IDE.ToolServer.Types
import qualified IHP.IDE.PortConfig as PortConfig
import IHP.IDE.Types
import qualified Network.Socket as Socket
import qualified System.Process as Process
import System.Info (os)
import qualified System.Environment as Env
import IHP.Controller.Context
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text as Text
import System.Directory
import qualified Data.Text.IO as IO

-- | Returns the port used by the running app. Usually returns @8000@.
appPort :: (?context :: ControllerContext) => Socket.PortNumber
appPort = (unsafePerformIO (fromContext @ToolServerApplication))
        |> get #devServerContext
        |> get #portConfig
        |> get #appPort

openEditor :: Text -> Int -> Int -> IO ()
openEditor path line col = do
    (supportsLineAndCol, editor) <- findEditor
    let command =
            editor <> " " <> path <> if supportsLineAndCol then ":" <> tshow line <> ":" <> tshow col else ""
    _ <- Process.system (cs command)
    unless supportsLineAndCol (putStrLn "Pro Tip: Set the env var IHP_EDITOR to your favorite editor. Then all your files will be opened at the right line and column where the error is reported.")
    pure ()

-- | Returns the editor command for the user and also whether the command supports line and col notation
--
-- Line and col notation means that calling @editor myfile.hs:10:5@ works. Tools like @xdg-open@ or on macOS @open@
-- don't support this notation and thus need to be called like @xdg-open myfile.hs@ instead of @xdg-open myfile.hs:10:5@
--
-- Looks for a the env vars IHP_EDITOR or EDITOR. As fallback it uses @open@ or @xdg-open@ (depends on OS).
--
findEditor :: IO (Bool, Text)
findEditor = do
    ihpEditorEnv <- Env.lookupEnv "IHP_EDITOR"
    editorEnv <- Env.lookupEnv "EDITOR"
    pure case catMaybes [ihpEditorEnv, editorEnv] of
        (editor:_) -> (True, cs editor)
        [] -> case os of
            "linux" -> (False, "xdg-open")
            "darwin" -> (False, "open")


findWebControllers :: IO [Text]
findWebControllers = do
    directoryFiles <-  listDirectory "Web/Controller"
    let controllerFiles :: [Text] =  filter (\x -> not $ "Prelude" `isInfixOf` x || "Context" `isInfixOf` x)  $ map cs directoryFiles
    pure $ map (Text.replace ".hs" "") controllerFiles

findControllers :: Text -> IO [Text]
findControllers application = do
    directoryFiles <-  listDirectory $ cs $ application <> "/Controller"
    let controllerFiles :: [Text] =  filter (\x -> not $ "Prelude" `isInfixOf` x || "Context" `isInfixOf` x)  $ map cs directoryFiles
    pure $ map (Text.replace ".hs" "") controllerFiles

findApplications :: IO ([Text])
findApplications = do
    mainhs <- IO.readFile "Main.hs"
    let imports = filter (\line -> "import " `isPrefixOf` line && ".FrontController" `isSuffixOf` line) (lines mainhs)
    pure (map removeImport imports)
        where
            removeImport line = Text.replace ".FrontController" "" (Text.replace "import " "" line)

theDevServerContext :: (?context :: ControllerContext) => IO Context
theDevServerContext = get #devServerContext <$> (fromContext @ToolServerApplication)

clearDatabaseNeedsMigration :: (?context :: ControllerContext) => IO ()
clearDatabaseNeedsMigration = do
    context <- theDevServerContext
    state <- readIORef (get #appStateRef context)
    writeIORef (get #databaseNeedsMigration state) False
    pure ()

markDatabaseNeedsMigration :: (?context :: ControllerContext) => IO ()
markDatabaseNeedsMigration = do
    context <- theDevServerContext
    state <- readIORef (get #appStateRef context)
    writeIORef (get #databaseNeedsMigration state) True
    pure ()