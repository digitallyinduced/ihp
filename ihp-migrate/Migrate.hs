module Main where

import Prelude
import IHP.SchemaMigration
import Main.Utf8 (withUtf8)
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Read (readMaybe)
import Data.String.Conversions (cs)
import Control.Exception (bracket)
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as ConnectionSettings

main :: IO ()
main = withUtf8 do
    databaseUrl <- lookupEnv "DATABASE_URL" >>= maybe (die "DATABASE_URL not set") pure
    bracket (acquireConnection databaseUrl) Connection.release \connection -> do
        let ?connection = connection
        minimumRevision <- fmap (>>= readMaybe) (lookupEnv "MINIMUM_REVISION")
        migrate MigrateOptions { minimumRevision }

acquireConnection :: String -> IO Connection.Connection
acquireConnection databaseUrl = do
    result <- Connection.acquire (ConnectionSettings.connectionString (cs databaseUrl))
    case result of
        Right connection -> pure connection
        Left err -> die ("Failed to connect to database: " <> show err)
