module Main where

import Prelude
import IHP.SchemaMigration
import Main.Utf8 (withUtf8)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import Text.Read (readMaybe)
import Data.String.Conversions (cs)
import qualified Data.Text.IO as Text
import System.IO (stderr)
import Control.Exception (bracket)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings

main :: IO ()
main = withUtf8 do
    arguments <- getArgs
    case arguments of
        [] -> runMigrations
        ["--extract-extensions", schemaPath] -> extractExtensions schemaPath
        _ -> die "Usage: migrate [--extract-extensions SCHEMA.sql]"

runMigrations :: IO ()
runMigrations = do
    databaseUrl <- lookupEnv "DATABASE_URL" >>= maybe (die "DATABASE_URL not set") pure
    adminDatabaseUrl <- lookupEnv "DATABASE_ADMIN_URL"
    bracket (acquireConnection "DATABASE_URL" databaseUrl) Connection.release \connection -> do
        withAdminConnectionProvider adminDatabaseUrl \getAdminConnection -> do
            minimumRevision <- fmap (>>= readMaybe) (lookupEnv "MINIMUM_REVISION")
            result <- migrateWithAdminConnectionProvider connection getAdminConnection MigrateOptions { minimumRevision }
            case result of
                Left err -> do
                    Text.hPutStrLn stderr err
                    exitFailure
                Right () -> pure ()

withAdminConnectionProvider :: Maybe String -> (IO (Maybe Connection.Connection) -> IO a) -> IO a
withAdminConnectionProvider Nothing action = action (pure Nothing)
withAdminConnectionProvider (Just databaseUrl) action =
    bracket (newIORef Nothing) releaseAdminConnection \connectionReference ->
        action (Just <$> acquireAdminConnection connectionReference databaseUrl)

acquireAdminConnection :: IORef (Maybe Connection.Connection) -> String -> IO Connection.Connection
acquireAdminConnection connectionReference databaseUrl = do
    currentConnection <- readIORef connectionReference
    case currentConnection of
        Just connection -> pure connection
        Nothing -> do
            connection <- acquireConnection "DATABASE_ADMIN_URL" databaseUrl
            writeIORef connectionReference (Just connection)
            pure connection

releaseAdminConnection :: IORef (Maybe Connection.Connection) -> IO ()
releaseAdminConnection connectionReference = do
    connection <- readIORef connectionReference
    maybe (pure ()) Connection.release connection

extractExtensions :: FilePath -> IO ()
extractExtensions schemaPath = do
    schemaSql <- Text.readFile schemaPath
    case extractCreateExtensionsSql schemaSql of
        Left errorMessage -> die (cs errorMessage)
        Right extensionSql -> Text.putStr extensionSql

acquireConnection :: String -> String -> IO Connection.Connection
acquireConnection variableName databaseUrl = do
    result <- Connection.acquire (ConnectionSettings.connectionString (cs databaseUrl))
    case result of
        Right connection -> pure connection
        Left err -> die ("Failed to connect using " <> variableName <> ": " <> show err)
