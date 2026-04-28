{-|
Helpers for the split web/worker dev mode.

The dev server uses two GHCi sessions in separate processes:

* @web@ ('IHP.IDE.DevServer') runs the user's @Main.hs@ — only the web server.
* @worker@ ('IHP.IDE.DevWorker') runs a generated @build\/RunJobs.hs@ — only the job workers.

This module exposes the small bits both binaries share: detecting whether the
project has any jobs, generating the worker entry module, and the path the two
processes use for IPC.
-}
module IHP.IDE.SplitMode
( hasJobs
, generateRunJobsModule
, workerSocketPath
, runJobsModulePath
, runJobsModuleContents
) where

import ClassyPrelude
import qualified Data.ByteString as ByteString
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Data.Char as Char

-- | Path to the Unix domain socket used by the web process to signal the worker
-- process that it should reload.
workerSocketPath :: FilePath
workerSocketPath = "build/.dev-worker.sock"

-- | Path to the generated worker entry module.
runJobsModulePath :: FilePath
runJobsModulePath = "build/RunJobs.hs"

-- | Detect whether the current project has any job modules.
--
-- A project has jobs if any @.hs@ file lives under a directory whose name
-- (case-insensitive) is exactly @Job@. This mirrors the @hasJobs@ predicate
-- used by the production nix build at @NixSupport\/default.nix@.
hasJobs :: IO Bool
hasJobs = anyJobHsIn "."

anyJobHsIn :: FilePath -> IO Bool
anyJobHsIn dir = do
    entries <- Directory.listDirectory dir `catchAny` \_ -> pure []
    let isInJobDir = isJobDirName (FilePath.takeFileName dir)
    let here = if isInJobDir
            then any (\name -> ".hs" `isSuffixOf` (pack name :: Text)) entries
            else False
    if here
        then pure True
        else do
            let children = map (dir FilePath.</>) entries
            subdirs <- filterM keepDir children
            anyM anyJobHsIn subdirs

isJobDirName :: FilePath -> Bool
isJobDirName name = map Char.toLower name == "job"

-- | Skip directories that would just slow us down or pull in unrelated trees.
keepDir :: FilePath -> IO Bool
keepDir path = do
    isDir <- Directory.doesDirectoryExist path
    pure (isDir && not (isExcluded (FilePath.takeFileName path)))

isExcluded :: FilePath -> Bool
isExcluded name = name `elem`
    [ ".devenv", ".direnv", ".git", "build", "dist", "dist-newstyle"
    , "node_modules", "static", ".devbox", ".cabal-sandbox"
    ]

-- | Idempotently write @build\/RunJobs.hs@. If the file already contains the
-- expected content, this is a no-op (mtime is preserved so the file watcher
-- isn't self-triggered).
generateRunJobsModule :: IO ()
generateRunJobsModule = do
    Directory.createDirectoryIfMissing True "build"
    let path = runJobsModulePath
    let want = runJobsModuleContents
    have <- (Just <$> ByteString.readFile path) `catchAny` \_ -> pure Nothing
    case have of
        Just existing | existing == want -> pure ()
        _ -> ByteString.writeFile path want

-- | The exact bytes written to @build\/RunJobs.hs@. Mirrors the template in
-- @NixSupport\/default.nix@ used by the production build.
runJobsModuleContents :: ByteString
runJobsModuleContents =
    "module RunJobs (main) where\n\
    \import Application.Script.Prelude\n\
    \import IHP.ScriptSupport\n\
    \import IHP.Job.Runner\n\
    \import qualified Config\n\
    \import Application.Worker ()\n\
    \main :: IO ()\n\
    \main = runScript Config.config (runJobWorkers (workers RootApplication))\n"

-- | Helper used by 'anyJobHsIn'.
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM f (x:xs) = do
    result <- f x
    if result then pure True else anyM f xs
