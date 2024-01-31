module IHP.IDE.FileWatcher (withFileWatcher) where

import IHP.Prelude
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (filterM)
import System.Directory (listDirectory, doesDirectoryExist)
import qualified Data.Map as Map
import qualified System.FSNotify as FS
import IHP.IDE.Types
import qualified Data.Time.Clock as Clock
import qualified Data.List as List
import IHP.IDE.LiveReloadNotificationServer (notifyAssetChange)
import qualified Control.Debounce as Debounce

withFileWatcher :: (?context :: Context) => IO () -> IO ()
withFileWatcher inner =
        withAsync callback \_ -> inner
    where
        callback = do
            dispatchHaskellFileChanged <- Debounce.mkDebounce Debounce.defaultDebounceSettings
                     { Debounce.debounceAction = dispatch HaskellFileChanged
                     , Debounce.debounceFreq = 50000 -- 50ms
                     , Debounce.debounceEdge = Debounce.leadingEdge
                     }
            dispatchSchemaChanged <- Debounce.mkDebounce Debounce.defaultDebounceSettings
                     { Debounce.debounceAction = dispatch SchemaChanged
                     , Debounce.debounceFreq = 50000 -- 50ms
                     , Debounce.debounceEdge = Debounce.leadingEdge
                     }
            let
                handleFileChangeDebounced :: FS.Event -> IO ()
                handleFileChangeDebounced = handleFileChange dispatchHaskellFileChanged dispatchSchemaChanged
            FS.withManagerConf fileWatcherConfig \manager -> do
                state <- newFileWatcherState
                watchRootDirectoryFiles handleFileChangeDebounced manager state
                watchSubDirectories handleFileChangeDebounced manager state
                forever (threadDelay maxBound) `finally` FS.stopManager manager

        watchRootDirectoryFiles handleFileChange manager state = 
                FS.watchDir manager "." shouldActOnRootFileChange (handleRootFileChange handleFileChange manager state)
        
        watchSubDirectories handleFileChange manager state = do
                directories <- listWatchableDirectories
                forM_ directories \directory -> do
                    startWatchingSubDirectory handleFileChange manager state directory

type WatchedDirectories = Map FilePath FS.StopListening

type FileWatcherState = MVar WatchedDirectories

newFileWatcherState :: IO FileWatcherState
newFileWatcherState = newMVar mempty

startWatchingSubDirectory :: (FS.Event -> IO ()) -> FS.WatchManager -> FileWatcherState -> FilePath -> IO ()
startWatchingSubDirectory handleFileChange manager state path = do
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
fileWatcherConfig = FS.defaultConfig

handleFileChange :: (?context :: Context) => IO () -> IO () -> FS.Event -> IO ()
handleFileChange dispatchHaskellFileChanged dispatchSchemaChanged event = do
    let filePath = event.eventPath
    if isHaskellFile filePath
        then dispatchHaskellFileChanged
        else if isSchemaSQL filePath
            then dispatchSchemaChanged
            else if isAssetFile filePath
                then notifyAssetChange
                else mempty
                  
handleRootFileChange :: (FS.Event -> IO ()) -> FS.WatchManager -> FileWatcherState -> FS.Event -> IO ()                 
handleRootFileChange handleFileChange manager state event =
    case event of
        FS.Added filePath _ true ->
            if isDirectoryWatchable filePath then do
                startWatchingSubDirectory handleFileChange manager state filePath
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