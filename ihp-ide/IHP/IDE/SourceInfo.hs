{-|
Module: IHP.IDE.SourceInfo
Description: Detect and display IHP source location info at startup
-}
module IHP.IDE.SourceInfo where

import IHP.Prelude
import qualified System.Process as Process
import qualified System.Environment as Env
import qualified System.FilePath as FilePath
import qualified Control.Exception.Safe as Exception
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS

data SourceInfo
    = NixStore { nixStorePath :: Text, flakeBranch :: Maybe Text, flakeRevision :: Maybe Text }
    | LocalCheckout { path :: Text, gitBranch :: Maybe Text, gitCommit :: Maybe Text }
    deriving (Show, Eq)

getSourceInfo :: IO SourceInfo
getSourceInfo = do
    -- Use IHP_LIB env var to detect source location.
    -- IHP_LIB is set in both nix store and local checkout scenarios,
    -- so we check whether the path is inside /nix/store/ to distinguish.
    ihpLib <- Env.lookupEnv "IHP_LIB"
    case ihpLib of
        Just lib
            | "/nix/store/" `List.isPrefixOf` lib -> do
                (branch, rev) <- getFlakeLockInfo
                pure (NixStore (cs lib) branch rev)
            | otherwise -> do
                let ihpRoot = FilePath.takeDirectory (FilePath.takeDirectory (FilePath.takeDirectory lib))
                gitBranch <- getGitInfo ihpRoot "rev-parse" ["--abbrev-ref", "HEAD"]
                gitCommit <- getGitInfo ihpRoot "rev-parse" ["--short", "HEAD"]
                pure (LocalCheckout (cs ihpRoot) gitBranch gitCommit)
        Nothing -> do
            exePath <- Env.getExecutablePath
            if "/nix/store/" `List.isPrefixOf` exePath
                then do
                    (branch, rev) <- getFlakeLockInfo
                    pure (NixStore (cs (FilePath.takeDirectory exePath)) branch rev)
                else do
                    let dir = FilePath.takeDirectory exePath
                    gitBranch <- getGitInfo dir "rev-parse" ["--abbrev-ref", "HEAD"]
                    gitCommit <- getGitInfo dir "rev-parse" ["--short", "HEAD"]
                    pure (LocalCheckout (cs dir) gitBranch gitCommit)

-- | Parse flake.lock in the current directory to extract the ihp input's branch and revision.
getFlakeLockInfo :: IO (Maybe Text, Maybe Text)
getFlakeLockInfo = do
    result <- Exception.tryAny do
        contents <- LBS.readFile "flake.lock"
        case Aeson.decode contents of
            Just (Aeson.Object root) -> do
                let branch = do
                        Aeson.Object nodes <- KeyMap.lookup "nodes" root
                        Aeson.Object ihp <- KeyMap.lookup "ihp" nodes
                        Aeson.Object original <- KeyMap.lookup "original" ihp
                        Aeson.String ref <- KeyMap.lookup "ref" original
                        pure ref
                let rev = do
                        Aeson.Object nodes <- KeyMap.lookup "nodes" root
                        Aeson.Object ihp <- KeyMap.lookup "ihp" nodes
                        Aeson.Object locked <- KeyMap.lookup "locked" ihp
                        Aeson.String r <- KeyMap.lookup "rev" locked
                        pure r
                pure (branch, rev)
            _ -> pure (Nothing, Nothing)
    case result of
        Right val -> pure val
        Left _ -> pure (Nothing, Nothing)

getGitInfo :: FilePath -> String -> [String] -> IO (Maybe Text)
getGitInfo dir subcommand args = do
    result <- Exception.tryAny do
        let process = (Process.proc "git" (["-C", dir, subcommand] <> args))
                { Process.std_err = Process.CreatePipe }
        output <- Process.readCreateProcess process ""
        pure (Text.strip (cs output))
    case result of
        Right val -> pure (Just val)
        Left _ -> pure Nothing

formatSourceInfo :: SourceInfo -> Text
formatSourceInfo (NixStore storePath branch rev) =
    "IHP Source: Nix store (" <> storePath <> ")" <> branchInfo <> revInfo
  where
    branchInfo = case branch of
        Just b -> " on branch " <> b
        Nothing -> ""
    revInfo = case rev of
        Just r -> " at revision " <> r
        Nothing -> ""
formatSourceInfo (LocalCheckout path gitBranch gitCommit) =
    "IHP Source: Local checkout (" <> path <> ")" <> branchInfo <> commitInfo
  where
    branchInfo = case gitBranch of
        Just branch -> " on branch " <> branch
        Nothing -> ""
    commitInfo = case gitCommit of
        Just commit -> " at commit " <> commit
        Nothing -> ""
