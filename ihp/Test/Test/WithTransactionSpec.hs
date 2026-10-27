{-|
Module: Test.WithTransactionSpec
Description: Integration tests for 'withTransaction', in particular nested calls
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.WithTransactionSpec where

import Test.Hspec
import IHP.Prelude
import IHP.ModelSupport
import qualified Hasql.Pool as HasqlPool
import qualified Hasql.Session as Session
import qualified Database.PostgreSQL.Simple.Types as PG
import System.Environment (lookupEnv)
import qualified Control.Exception as Exception

tests :: Spec
tests = describe "withTransaction" do
    it "commits the work of the block" $ withDB \modelContext -> do
        let ?modelContext = modelContext
        withTransaction do
            insertItem "a"
        itemNames >>= (`shouldBe` ["a"])

    it "rolls back the work of the block when it throws" $ withDB \modelContext -> do
        let ?modelContext = modelContext
        (result :: Either Exception.ErrorCall ()) <- Exception.try $ withTransaction do
            insertItem "a"
            Exception.throwIO (Exception.ErrorCall "boom")
        result `shouldBe` Left (Exception.ErrorCall "boom")
        itemNames >>= (`shouldBe` [])

    describe "nested" do
        it "runs the inner block inside the outer transaction" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "outer"
                withTransaction do
                    insertItem "inner"
                -- Both rows are visible inside the outer transaction before it commits
                itemNames >>= (`shouldBe` ["inner", "outer"])
            itemNames >>= (`shouldBe` ["inner", "outer"])

        it "rolls back only the inner block when it throws and the outer block catches the exception" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "outer"
                (result :: Either Exception.ErrorCall ()) <- Exception.try $ withTransaction do
                    insertItem "inner"
                    Exception.throwIO (Exception.ErrorCall "boom")
                result `shouldBe` Left (Exception.ErrorCall "boom")
                insertItem "after"
            itemNames >>= (`shouldBe` ["after", "outer"])

        it "lets the outer block carry on after a failed SQL statement in the inner block" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "outer"
                result <- Exception.try @HasqlSessionError $ withTransaction do
                    insertItem "inner"
                    insertItem "outer" -- violates the primary key
                case result of
                    Left _ -> pure ()
                    Right () -> expectationFailure "expected the duplicate insert to fail"
                -- Without a savepoint, the whole transaction would be aborted here
                insertItem "after"
            itemNames >>= (`shouldBe` ["after", "outer"])

        it "rolls back the inner block and throws when it swallowed a failed SQL statement" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "outer"
                result <- Exception.try @HasqlSessionError $ withTransaction do
                    insertItem "inner"
                    (_ :: Either HasqlSessionError ()) <- Exception.try (insertItem "outer") -- violates the primary key
                    pure ()
                case result of
                    Left _ -> pure ()
                    Right () -> expectationFailure "expected the inner withTransaction to report the failed statement"
                -- The transaction is usable again, and the inner work is gone
                insertItem "after"
            itemNames >>= (`shouldBe` ["after", "outer"])

        it "rolls back its own work even when a savepoint was left behind inside the block" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "outer"
                (result :: Either Exception.ErrorCall ()) <- Exception.try $ withTransaction do
                    insertItem "middle"
                    -- What an inner withTransaction leaves behind when it is interrupted while
                    -- creating its savepoint. The middle rollback must not be redirected to it,
                    -- which is what happens when every level shares one savepoint name. This
                    -- is the name the first implementation used for every level.
                    unsafeSqlExecDiscardResult "SAVEPOINT ihp_with_transaction" ()
                    Exception.throwIO (Exception.ErrorCall "boom")
                result `shouldBe` Left (Exception.ErrorCall "boom")
                insertItem "after"
            itemNames >>= (`shouldBe` ["after", "outer"])

        it "rolls back the outer transaction when the inner exception is not caught" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            (result :: Either Exception.ErrorCall ()) <- Exception.try $ withTransaction do
                insertItem "outer"
                withTransaction do
                    insertItem "inner"
                    Exception.throwIO (Exception.ErrorCall "boom")
            result `shouldBe` Left (Exception.ErrorCall "boom")
            itemNames >>= (`shouldBe` [])

        it "supports more than one level of nesting" $ withDB \modelContext -> do
            let ?modelContext = modelContext
            withTransaction do
                insertItem "level1"
                withTransaction do
                    insertItem "level2"
                    (_ :: Either Exception.ErrorCall ()) <- Exception.try $ withTransaction do
                        insertItem "level3"
                        Exception.throwIO (Exception.ErrorCall "boom")
                    insertItem "level2-after"
            itemNames >>= (`shouldBe` ["level1", "level2", "level2-after"])

insertItem :: (?modelContext :: ModelContext) => Text -> IO ()
insertItem name = unsafeSqlExecDiscardResult "INSERT INTO with_transaction_spec_items (name) VALUES (?)" (PG.Only name)

itemNames :: (?modelContext :: ModelContext) => IO [Text]
itemNames = do
    rows <- unsafeSqlQuery "SELECT name FROM with_transaction_spec_items ORDER BY name" ()
    pure (map (\(PG.Only name) -> name) rows)

-- | Run a test with a database connection, creating/dropping a temp table.
-- Skips if PostgreSQL is not available.
withDB :: (ModelContext -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    let logger = noopLogger
    modelContext <- createModelContext databaseUrl logger
    let pool = modelContext.hasqlPool
    let setup = do
            result <- HasqlPool.use pool $ Session.script
                "DROP TABLE IF EXISTS with_transaction_spec_items;\
                \CREATE TABLE with_transaction_spec_items (name TEXT PRIMARY KEY);"
            case result of
                Left err -> Exception.throwIO (HasqlError err)
                Right () -> pure ()
    let teardown = do
            _ <- HasqlPool.use pool $ Session.script "DROP TABLE IF EXISTS with_transaction_spec_items;"
            releaseModelContext modelContext
    result <- Exception.try (setup >> action modelContext `Exception.finally` teardown)
    case result of
        Right () -> pure ()
        Left (HasqlError (HasqlPool.ConnectionUsageError _)) ->
            pendingWith "PostgreSQL not available (set DATABASE_URL or start a local Postgres)"
        Left e -> Exception.throwIO e
