{-# LANGUAGE ApplicativeDo #-}
{-|
Module: Test.FetchPipelinedSpec
Description: Integration tests for IHP.FetchPipelined
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.FetchPipelinedSpec where

import Test.Hspec
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ModelSupport (createModelContext, releaseModelContext, HasqlError(..), Table(..), GetModelByTableName)
import IHP.ModelSupport.Types (GetTableName, PrimaryKey, Id'(..))
import IHP.Hasql.FromRow (FromRowHasql(..), HasqlDecodeColumn(..))
import IHP.FetchPipelined
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as Session
import qualified IHP.Log as Log
import IHP.Log.Types (LogLevel(..), LoggerSettings(..))
import System.Environment (lookupEnv)
import qualified Control.Exception as Exception

-- Model mapping to a temp table created during tests

data FpItem = FpItem
    { id :: UUID
    , name :: Text
    , active :: Bool
    }
    deriving (Show, Eq)

type instance GetTableName FpItem = "fp_items"
type instance GetModelByTableName "fp_items" = FpItem
type instance PrimaryKey "fp_items" = UUID

instance Table FpItem where
    columnNames = ["id", "name", "active"]
    primaryKeyColumnNames = ["id"]

instance FromRowHasql FpItem where
    hasqlRowDecoder = FpItem
        <$> hasqlColumnDecoder
        <*> hasqlColumnDecoder
        <*> hasqlColumnDecoder

-- | Run a test with a database connection, creating/dropping a temp table.
-- Skips if PostgreSQL is not available.
withDB :: (ModelContext -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    logger <- Log.newLogger def { level = Warn }
    modelContext <- createModelContext databaseUrl logger
    let pool = modelContext.hasqlPool
    let setup = do
            result <- HasqlPool.use pool $ Session.script
                "CREATE TABLE IF NOT EXISTS fp_items (id UUID PRIMARY KEY, name TEXT NOT NULL, active BOOL NOT NULL);\
                \INSERT INTO fp_items (id, name, active) VALUES \
                \('a0000000-0000-0000-0000-000000000001', 'alpha', true),\
                \('a0000000-0000-0000-0000-000000000002', 'beta', true),\
                \('a0000000-0000-0000-0000-000000000003', 'gamma', false);"
            case result of
                Left err -> Exception.throwIO (HasqlError err)
                Right () -> pure ()
    let teardown = do
            _ <- HasqlPool.use pool $ Session.script "DROP TABLE IF EXISTS fp_items;"
            releaseModelContext modelContext
    result <- Exception.try (setup >> action modelContext `Exception.finally` teardown)
    case result of
        Right () -> pure ()
        Left (HasqlError (HasqlPool.ConnectionUsageError _)) ->
            pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
        Left e -> Exception.throwIO e

tests :: Spec
tests = do
    describe "IHP.FetchPipelined" do
        describe "fetchPipelined" do
            it "should fetch all rows with toPipelineStatement" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                items <- fetchPipelined $
                    query @FpItem |> toPipelineStatement
                length items `shouldBe` 3

            it "should apply filterWhere" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                items <- fetchPipelined $
                    query @FpItem |> filterWhere (#active, True) |> toPipelineStatement
                length items `shouldBe` 2

            it "should fetch multiple queries in one pipeline" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                (allItems, activeItems) <- fetchPipelined do
                    allItems <- query @FpItem |> toPipelineStatement
                    activeItems <- query @FpItem |> filterWhere (#active, True) |> toPipelineStatement
                    pure (allItems, activeItems)
                length allItems `shouldBe` 3
                length activeItems `shouldBe` 2

            it "should pipeline three queries" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                (items, count, exists) <- fetchPipelined do
                    items  <- query @FpItem |> filterWhere (#active, True) |> toPipelineStatement
                    count  <- query @FpItem |> toPipelineStatementCount
                    exists <- query @FpItem |> filterWhere (#active, False) |> toPipelineStatementExists
                    pure (items, count, exists)
                length items `shouldBe` 2
                count `shouldBe` 3
                exists `shouldBe` True

        describe "toPipelineStatementOneOrNothing" do
            it "should return Just for matching row" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                result <- fetchPipelined $
                    query @FpItem |> filterWhere (#name, "alpha" :: Text) |> toPipelineStatementOneOrNothing
                ((.name) <$> result) `shouldBe` Just "alpha"

            it "should return Nothing for no match" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                result <- fetchPipelined $
                    query @FpItem |> filterWhere (#name, "nonexistent" :: Text) |> toPipelineStatementOneOrNothing
                result `shouldBe` Nothing

        describe "toPipelineStatementCount" do
            it "should count all rows" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                count <- fetchPipelined $
                    query @FpItem |> toPipelineStatementCount
                count `shouldBe` 3

            it "should count filtered rows" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                count <- fetchPipelined $
                    query @FpItem |> filterWhere (#active, False) |> toPipelineStatementCount
                count `shouldBe` 1

        describe "toPipelineStatementExists" do
            it "should return True when rows exist" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                exists <- fetchPipelined $
                    query @FpItem |> toPipelineStatementExists
                exists `shouldBe` True

            it "should return False when no rows match" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                exists <- fetchPipelined $
                    query @FpItem |> filterWhere (#name, "nonexistent" :: Text) |> toPipelineStatementExists
                exists `shouldBe` False
