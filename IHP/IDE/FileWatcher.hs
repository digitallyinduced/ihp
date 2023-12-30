module IHP.IDE.FileWatcher (withFileWatcher) where

import IHP.Prelude
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (filterM)
import System.Directory (listDirectory, doesDirectoryExist)
import qualified Data.Map as Map
import qualified System.FSNotify as FS
import System.FSNotify.Streaming (Debounce(..))
import IHP.IDE.Types
import qualified Data.Time.Clock as Clock
import qualified Data.List as List
import IHP.IDE.LiveReloadNotificationServer (notifyAssetChange)

withFileWatcher :: (?context :: Context) => IO () -> IO ()
withFileWatcher inner = withAsync callback \_ -> inner
    where
        callback = FS.withManagerConf fileWatcherConfig \manager -> do
                state <- newFileWatcherState
                watchRootDirectoryFiles manager state
                watchSubDirectories manager state
                forever (threadDelay maxBound) `finally` FS.stopManager manager
        watchRootDirectoryFiles manager state =
                FS.watchDir manager "." shouldActOnRootFileChange (handleRootFileChange manager state)
        watchSubDirectories manager state = do
                directories <- listWatchableDirectories
                forM_ directories \directory -> do
                    startWatchingSubDirectory manager state directory

type WatchedDirectories = Map FilePath FS.StopListening

type FileWatcherState = MVar WatchedDirectories

newFileWatcherState :: IO FileWatcherState
newFileWatcherState = newMVar mempty

startWatchingSubDirectory :: (?context :: Context) => FS.WatchManager -> FileWatcherState -> FilePath -> IO ()
startWatchingSubDirectory manager state path = do
    watchedDirectories <- readMVar state
    case Map.lookup path watchedDirectories of
        Just _ -> pure ()
        Nothing -> do
            stop <- FS.watchTree manager path shouldActOnFileChange handleFileChange
            modifyMVar_ state (\map -> pure (Map.insert path stop map))

stopWatchingSubDirectory :: FileWatcherState -> FilePath -> IO ()
stopWatchingSubDirectory state path = do
    watchedDirectories <- readMVar state
    case Map.lookup path watchedDirectories of
        Just stop -> do
            stop
            modifyMVar_ state (\map -> pure (Map.delete path map))
        Nothing -> pure ()

listWatchableDirectories :: IO [String]
listWatchableDirectories = do
    rootDirectoryContents <- listDirectory "."
    filterM shouldWatchDirectory rootDirectoryContents

shouldWatchDirectory :: String -> IO Bool
shouldWatchDirectory path = do
    isDirectory <- doesDirectoryExist path
    pure $ isDirectory && isDirectoryWatchable path

isDirectoryWatchable :: String -> Bool
isDirectoryWatchable path =
    path /= ".devenv" && path /= ".direnv"

fileWatcherConfig :: FS.WatchConfig
fileWatcherConfig = FS.defaultConfig { FS.debounce = Debounce 0.1 }

handleFileChange :: (?context :: Context) => FS.Event -> IO ()
handleFileChange event = do
    let filePath = event.eventPath
    if isHaskellFile filePath
        then dispatch HaskellFileChanged
        else if isSchemaSQL filePath
            then dispatch SchemaChanged
            else if isAssetFile filePath
                then notifyAssetChange
                else mempty

handleRootFileChange :: (?context :: Context) => FS.WatchManager -> FileWatcherState -> FS.Event -> IO ()
handleRootFileChange manager state event =
    case event of
        FS.Added filePath _ true ->
            if isDirectoryWatchable filePath then do
                startWatchingSubDirectory manager state filePath
            else pure ()
        FS.Removed filePath _ true ->
            stopWatchingSubDirectory state filePath
        _ ->
            handleFileChange event

shouldActOnRootFileChange :: FS.ActionPredicate
shouldActOnRootFileChange event =
    if FS.eventIsDirectory event == FS.IsDirectory
    then isDirectoryWatchable event.eventPath
    else shouldActOnFileChange event

shouldActOnFileChange :: FS.ActionPredicate
shouldActOnFileChange event =
    let path = event.eventPath
    in isHaskellFile path || isAssetFile path || isSQLFile path

isHaskellFile :: String -> Bool
isHaskellFile = List.isSuffixOf ".hs"

isAssetFile :: String -> Bool
isAssetFile = List.isSuffixOf ".css"

isSQLFile :: String -> Bool
isSQLFile = List.isSuffixOf ".sql"

isSchemaSQL :: String -> Bool
isSchemaSQL = List.isSuffixOf "Application/Schema.sql"