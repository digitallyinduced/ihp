module Test.Pagination.ControllerFunctionsSpec where

import IHP.Prelude
import Test.Hspec
import IHP.Pagination.ControllerFunctions
import IHP.Pagination.Types (Options(..), Pagination(..))
import IHP.Controller.Context
import IHP.ModelSupport (createModelContext, releaseModelContext)
import qualified IHP.Log as Log
import IHP.Log.Types (LogLevel(..), LoggerSettings(..))
import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)
import qualified Data.Vault.Lazy as Vault
import qualified Data.TMap as TypeMap
import qualified Network.Wai as Wai
import qualified Database.PostgreSQL.Simple.Types as PG
import System.Environment (lookupEnv)
import qualified Control.Exception as Exception

tests :: Spec
tests = do
    describe "IHP.Pagination.ControllerFunctions" do
        describe "paginatedSqlQueryWithOptions" do
            it "should return first page with default options" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams []
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 100) AS n"
                        ()

                length results `shouldBe` 50
                pagination.currentPage `shouldBe` 1
                pagination.totalItems `shouldBe` 100
                pagination.pageSize `shouldBe` 50

            it "should return second page" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams [("page", "2")]
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 100) AS n"
                        ()

                length results `shouldBe` 50
                pagination.currentPage `shouldBe` 2
                pagination.totalItems `shouldBe` 100
                -- Second page should have items 51-100
                case results of
                    (PG.Only first : _) -> first `shouldBe` 51
                    _ -> expectationFailure "Expected non-empty results"

            it "should respect maxItems from request param" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams [("maxItems", "10")]
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 100) AS n"
                        ()

                length results `shouldBe` 10
                pagination.pageSize `shouldBe` 10
                pagination.totalItems `shouldBe` 100

            it "should respect custom options maxItems" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams []
                let ?request = ?context.request
                let options = Options { maxItems = 25, windowSize = 3 }

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        options
                        "SELECT generate_series(1, 100) AS n"
                        ()

                length results `shouldBe` 25
                pagination.pageSize `shouldBe` 25
                pagination.window `shouldBe` 3

            it "should cap maxItems at 200" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams [("maxItems", "9999")]
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 500) AS n"
                        ()

                length results `shouldBe` 200
                pagination.pageSize `shouldBe` 200
                pagination.totalItems `shouldBe` 500

            it "should return empty results for page beyond data" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams [("page", "100")]
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 10) AS n"
                        ()

                length results `shouldBe` 0
                pagination.totalItems `shouldBe` 10
                pagination.currentPage `shouldBe` 100

            it "should handle page + maxItems together" $ withDB \modelContext -> do
                let ?modelContext = modelContext
                let ?context = contextWithParams [("page", "3"), ("maxItems", "10")]
                let ?request = ?context.request

                (results :: [PG.Only Int], pagination) <-
                    paginatedSqlQueryWithOptions
                        defaultPaginationOptions
                        "SELECT generate_series(1, 100) AS n"
                        ()

                length results `shouldBe` 10
                pagination.currentPage `shouldBe` 3
                pagination.pageSize `shouldBe` 10
                -- Page 3 with pageSize 10 should start at item 21
                case results of
                    (PG.Only first : _) -> first `shouldBe` 21
                    _ -> expectationFailure "Expected non-empty results"

-- | Create a ControllerContext with the given request params
contextWithParams :: [(ByteString, ByteString)] -> ControllerContext
contextWithParams params =
    let requestBody = FormBody { params, files = [], rawPayload = "" }
        request = Wai.defaultRequest { Wai.vault = Vault.insert requestBodyVaultKey requestBody Vault.empty }
        customFields = TypeMap.insert request TypeMap.empty
    in FrozenControllerContext { customFields }

-- | Run a test with a database connection, skipping if PostgreSQL is not available
withDB :: (ModelContext -> IO ()) -> IO ()
withDB action = do
    envUrl <- lookupEnv "DATABASE_URL"
    let databaseUrl = maybe "postgresql:///postgres" cs envUrl
    result <- Exception.try do
        logger <- Log.newLogger def { level = Warn }
        Exception.bracket (createModelContext databaseUrl logger) releaseModelContext action
    case result of
        Right () -> pure ()
        Left (e :: Exception.SomeException) -> pendingWith (cs ("PostgreSQL not available: " <> tshow e))
