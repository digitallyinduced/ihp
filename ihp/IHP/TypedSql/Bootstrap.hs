module IHP.TypedSql.Bootstrap
    ( describeUsingBootstrap
    ) where

import           Control.Exception          (bracket_)
import           Control.Monad              (when)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8     as BS8
import           Data.Maybe                 (catMaybes)
import qualified Data.String.Conversions   as CS
import           System.Directory           (canonicalizePath, createDirectoryIfMissing,
                                             doesDirectoryExist, doesFileExist,
                                             findExecutable, removeDirectoryRecursive)
import           System.Environment         (lookupEnv)
import           System.FilePath            (isRelative, takeDirectory, takeFileName, (</>))
import           System.IO                  (Handle, hIsEOF)
import           System.IO.Temp             (withSystemTempDirectory)
import qualified System.Process             as Process

import           IHP.Prelude
import           IHP.TypedSql.Metadata      (DescribeResult, describeStatementWith)

data BootstrapConfig = BootstrapConfig
    { bcAppSchemaPath :: !FilePath
    , bcIhpSchemaPath :: !(Maybe FilePath)
    }

data PgTools = PgTools
    { pgInitdb   :: !FilePath
    , pgPostgres :: !FilePath
    , pgCreatedb :: !FilePath
    , pgPsql     :: !FilePath
    }

describeUsingBootstrap :: FilePath -> String -> IO DescribeResult
describeUsingBootstrap sourcePath sqlText = do
    config <- resolveBootstrapConfig sourcePath
    withBootstrapDatabase config \dbUrl ->
        describeStatementWith dbUrl (CS.cs sqlText)

resolveBootstrapConfig :: FilePath -> IO BootstrapConfig
resolveBootstrapConfig sourcePath = do
    sourceDir <- canonicalizePath (takeDirectory sourcePath)
    appSchemaPath <- resolveSchemaPath sourceDir
    ihpSchemaPath <- resolveIhpSchemaPath sourceDir
    pure BootstrapConfig
        { bcAppSchemaPath = appSchemaPath
        , bcIhpSchemaPath = ihpSchemaPath
        }

resolveSchemaPath :: FilePath -> IO FilePath
resolveSchemaPath sourceDir = do
    envSchema <- lookupEnv "IHP_TYPED_SQL_SCHEMA"
    case envSchema of
        Just path -> resolveRelativePath sourceDir path >>= ensureFileExists "IHP_TYPED_SQL_SCHEMA"
        Nothing -> do
            findUpwards sourceDir ("Application" </> "Schema.sql") >>= \case
                Just found -> pure found
                Nothing ->
                    fail "typedSql: could not find Application/Schema.sql. Set IHP_TYPED_SQL_SCHEMA to an absolute path."

resolveIhpSchemaPath :: FilePath -> IO (Maybe FilePath)
resolveIhpSchemaPath sourceDir = do
    envSchema <- lookupEnv "IHP_TYPED_SQL_IHP_SCHEMA"
    case envSchema of
        Just path -> Just <$> (resolveRelativePath sourceDir path >>= ensureFileExists "IHP_TYPED_SQL_IHP_SCHEMA")
        Nothing -> do
            envLib <- lookupEnv "IHP_LIB"
            fromLib <- case envLib of
                Just libPath -> do
                    let candidate = libPath </> "IHPSchema.sql"
                    exists <- doesFileExist candidate
                    pure (if exists then Just candidate else Nothing)
                Nothing -> pure Nothing
            case fromLib of
                Just _ -> pure fromLib
                Nothing -> findUpwards sourceDir ("ihp-ide" </> "data" </> "IHPSchema.sql")

resolveRelativePath :: FilePath -> FilePath -> IO FilePath
resolveRelativePath baseDir path = do
    let resolved = if isRelative path then baseDir </> path else path
    canonicalizePath resolved

ensureFileExists :: String -> FilePath -> IO FilePath
ensureFileExists label path = do
    exists <- doesFileExist path
    if exists
        then pure path
        else fail ("typedSql: " <> label <> " points to missing file: " <> path)

findUpwards :: FilePath -> FilePath -> IO (Maybe FilePath)
findUpwards startDir relativePath = go startDir
  where
    go current = do
        let candidate = current </> relativePath
        exists <- doesFileExist candidate
        if exists
            then Just <$> canonicalizePath candidate
            else do
                let parent = takeDirectory current
                if parent == current
                    then pure Nothing
                    else go parent

withBootstrapDatabase :: BootstrapConfig -> (BS.ByteString -> IO a) -> IO a
withBootstrapDatabase BootstrapConfig { bcAppSchemaPath, bcIhpSchemaPath } action = do
    PgTools { pgInitdb, pgPostgres, pgCreatedb, pgPsql } <- resolvePgTools
    withSystemTempDirectory "ihp-typed-sql" \tempDir -> do
        let dataDir = tempDir </> "state"
        let socketDir = "/tmp" </> takeFileName tempDir
        let cleanupSocket = do
                exists <- doesDirectoryExist socketDir
                when exists (removeDirectoryRecursive socketDir)
        bracket_ (createDirectoryIfMissing True socketDir) cleanupSocket do
            Process.callProcess pgInitdb [dataDir, "--no-locale", "--encoding", "UTF8"]

            let params =
                    (Process.proc pgPostgres ["-D", dataDir, "-k", socketDir, "-c", "listen_addresses="])
                        { Process.std_in = Process.CreatePipe
                        , Process.std_out = Process.CreatePipe
                        , Process.std_err = Process.CreatePipe
                        }
            Process.withCreateProcess params \_ _ stderrHandle processHandle -> do
                errHandle <- maybe (fail "typedSql: unable to read postgres logs") pure stderrHandle
                let stop = do
                        Process.terminateProcess processHandle
                        _ <- Process.waitForProcess processHandle
                        pure ()
                let start = do
                        waitUntilReady errHandle
                        Process.callProcess pgCreatedb ["app", "-h", socketDir]
                        let loadSchema file = Process.callProcess pgPsql ["-h", socketDir, "-d", "app", "-v", "ON_ERROR_STOP=1", "-f", file]
                        forM_ (catMaybes [bcIhpSchemaPath, Just bcAppSchemaPath]) loadSchema
                bracket_ start stop do
                    let dbUrl = CS.cs ("postgresql:///app?host=" <> socketDir)
                    action dbUrl

resolvePgTools :: IO PgTools
resolvePgTools = do
    pgPostgres <- requireExecutable "postgres"
    let binDir = takeDirectory pgPostgres
    pgInitdb <- findInBinOrPath binDir "initdb"
    pgCreatedb <- findInBinOrPath binDir "createdb"
    pgPsql <- findInBinOrPath binDir "psql"
    pure PgTools { pgInitdb, pgPostgres, pgCreatedb, pgPsql }

findInBinOrPath :: FilePath -> String -> IO FilePath
findInBinOrPath binDir name = do
    let candidate = binDir </> name
    exists <- doesFileExist candidate
    if exists then pure candidate else requireExecutable name

requireExecutable :: String -> IO FilePath
requireExecutable name =
    findExecutable name >>= \case
        Just path -> pure path
        Nothing -> fail ("typedSql: bootstrap requires '" <> name <> "' in PATH")

waitUntilReady :: Handle -> IO ()
waitUntilReady handle = do
    done <- hIsEOF handle
    if done
        then fail "typedSql: postgres exited before it was ready"
        else do
            line <- BS8.hGetLine handle
            if "database system is ready to accept connections" `BS8.isInfixOf` line
                then pure ()
                else waitUntilReady handle
