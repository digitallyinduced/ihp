module IHP.IDE.FileWatcher (withFileWatcher) where

import IHP.Prelude
import Control.Exception
import Control.Concurrent (threadDelay, myThreadId)
import qualified System.FSNotify as FS
import IHP.IDE.Types
import qualified Data.Time.Clock as Clock
import qualified Data.List as List
import IHP.IDE.LiveReloadNotificationServer (notifyAssetChange)

withFileWatcher :: (?context :: Context) => IO () -> IO ()
withFileWatcher inner = withAsync callback \_ -> inner
    where
        callback = FS.withManagerConf fileWatcherConfig \manager -> do
                FS.watchTree manager "." shouldActOnFileChange handleFileChange
                forever (threadDelay maxBound) `finally` FS.stopManager manager

fileWatcherDebounceTime :: NominalDiffTime
fileWatcherDebounceTime = Clock.secondsToNominalDiffTime 0.1 -- 100ms

fileWatcherConfig :: FS.WatchConfig
fileWatcherConfig = FS.defaultConfig { FS.confDebounce = FS.Debounce fileWatcherDebounceTime }

handleFileChange :: (?context :: Context) => FS.Event -> IO ()
handleFileChange event = do
    let filePath = getEventFilePath event
    if isHaskellFile filePath
        then dispatch HaskellFileChanged
        else if isSchemaSQL filePath
            then dispatch SchemaChanged
            else if isAssetFile filePath
                then notifyAssetChange
                else mempty

shouldActOnFileChange :: FS.ActionPredicate
shouldActOnFileChange event =
    let path = getEventFilePath event
    in isHaskellFile path || isAssetFile path || isSQLFile path

isHaskellFile :: String -> Bool
isHaskellFile = List.isSuffixOf ".hs"

isAssetFile :: String -> Bool
isAssetFile = List.isSuffixOf ".css"

isSQLFile :: String -> Bool
isSQLFile = List.isSuffixOf ".sql"

isSchemaSQL :: String -> Bool
isSchemaSQL = List.isSuffixOf "Application/Schema.sql"

getEventFilePath :: FS.Event -> FilePath
getEventFilePath event = case event of
        FS.Added filePath _ _ -> filePath
        FS.Modified filePath _ _ -> filePath
        FS.Removed filePath _ _ -> filePath
        FS.Unknown filePath _ _ -> filePath