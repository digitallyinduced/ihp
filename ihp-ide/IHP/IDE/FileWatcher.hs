module IHP.IDE.FileWatcher (runFileWatcherWithDebounce, FileWatcherParams (..)) where

import IHP.Prelude
import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad (filterM)
import System.Directory (listDirectory, doesDirectoryExist)
import qualified Data.Map as Map
import qualified System.FSNotify as FS
import qualified Data.List as List
import qualified Control.Debounce as Debounce

data FileWatcherParams
    = FileWatcherParams
    { onHaskellFileChanged :: IO ()
    , onSchemaChanged :: IO ()
    , onAssetChanged :: IO ()
    }

runFileWatcherWithDebounce :: FileWatcherParams -> IO ()
runFileWatcherWithDebounce params = do
    debouncedParams <- applyDebounce params
    runFileWatcher debouncedParams

applyDebounce :: FileWatcherParams -> IO FileWatcherParams
applyDebounce params = do
    onHaskellFileChangedDebounced <- Debounce.mkDebounce Debounce.defaultDebounceSettings
             { Debounce.debounceAction = params.onHaskellFileChanged
             , Debounce.debounceFreq = 50000 -- 50ms
             , Debounce.debounceEdge = Debounce.leadingEdge
             }
    onSchemaChangedDebounced <- Debounce.mkDebounce Debounce.defaultDebounceSettings
             { Debounce.debounceAction = params.onSchemaChanged
             , Debounce.debounceFreq = 50000 -- 50ms
             , Debounce.debounceEdge = Debounce.leadingEdge
             }

    pure params { onHaskellFileChanged = onHaskellFileChangedDebounced, onSchemaChanged = onSchemaChangedDebounced }

runFileWatcher :: FileWatcherParams -> IO ()
runFileWatcher params@FileWatcherParams { .. } = do
    FS.withManagerConf fileWatcherConfig \manager -> do
        state <- newFileWatcherState
        watchRootDirectoryFiles (handleFileChange params) manager state
        watchSubDirectories (handleFileChange params) manager state
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

handleFileChange :: FileWatcherParams -> FS.Event -> IO ()
handleFileChange params event = do
    let filePath = event.eventPath
    if isHaskellFile filePath
        then params.onHaskellFileChanged
        else if isSchemaSQL filePath
            then params.onSchemaChanged
            else if isAssetFile filePath
                then params.onAssetChanged
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