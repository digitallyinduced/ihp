module IHP.Test.Database where

import IHP.Prelude
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.FromRow as PG
import qualified Database.PostgreSQL.Simple.ToField as PG
import qualified Data.UUID.V4 as UUID
import qualified Data.UUID as UUID
import qualified System.Process as Process
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString


data TestDatabase = TestDatabase
    { name :: Text
    , url :: ByteString
    }

-- | Given a Postgres Database URL it creates a new randomly named database on the database server. Returns a database url to the freshly created database
--
-- >>> createTestDatabase "postgresql:///app?host=/myapp/build/db"
-- TestDatabase { name = "test-7d3bd463-4cce-413f-a272-ac52e5d93739", url = "postgresql:///test-7d3bd463-4cce-413f-a272-ac52e5d93739?host=/myapp/build/db" }
--
createTestDatabase :: ByteString -> IO TestDatabase
createTestDatabase databaseUrl = do
    databaseId <- UUID.nextRandom
    let databaseName = "test-" <> UUID.toText databaseId

    connection <- PG.connectPostgreSQL databaseUrl
    PG.execute connection "CREATE DATABASE ?" [PG.Identifier databaseName]

    let newUrl :: ByteString = databaseUrl
            |> cs
            |> Text.replace "postgresql:///app" ("postgresql:///" <> databaseName)
            |> cs

    importSql newUrl "Application/Schema.sql"

    pure TestDatabase { name = databaseName, url = newUrl }

-- | Given the master connection url and the open test database, this will clean up the test database
--
-- >>> deleteDatabase "postgresql:///app?host=/myapp/build/db" TestDatabase { name = "test-7d3bd463-4cce-413f-a272-ac52e5d93739", url = "postgresql:///test-7d3bd463-4cce-413f-a272-ac52e5d93739?host=/myapp/build/db" }
--
-- The master connection url needs to be passed as we cannot drop the database we're currently connected to, and therefore
-- we cannot use the test database itself.
--
deleteDatabase :: ByteString -> TestDatabase -> IO ()
deleteDatabase masterDatabaseUrl testDatabase = do
    connection <- PG.connectPostgreSQL masterDatabaseUrl

    -- The WITH FORCE is required to force close open connections
    -- Otherwise the DROP DATABASE takes a few seconds to execute
    PG.execute connection "DROP DATABASE ? WITH (FORCE)" [PG.Identifier (get #name testDatabase)]
    pure ()

importSql url file = do
    schemaSql <- ByteString.readFile file

    connection <- PG.connectPostgreSQL url
    PG.execute connection (PG.Query schemaSql) ()
    PG.close connection

