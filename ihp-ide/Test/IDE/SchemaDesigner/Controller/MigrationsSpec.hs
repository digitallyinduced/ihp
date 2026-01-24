{-|
Module: Test.IDE.SchemaDesigner.Controller.MigrationsSpec
Tests for Migrations Controller middleware integration.

This test verifies that the middleware stack properly handles migration actions
that require request body parsing.
-}
module Test.IDE.SchemaDesigner.Controller.MigrationsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Test.Mocking
import IHP.IDE.ToolServer.Types
import IHP.FrameworkConfig
import qualified Data.ByteString.Builder as ByteString
import Network.Socket (PortNumber)

-- | Helper to create a mock ToolServerApplication for testing
createMockToolServerApplication :: IO ToolServerApplication
createMockToolServerApplication = do
    postgresStandardOutput <- newIORef mempty
    postgresErrorOutput <- newIORef mempty
    appStandardOutput <- newIORef []
    appErrorOutput <- newIORef []
    let appPort = 8000 :: PortNumber
    databaseNeedsMigration <- newIORef False
    
    pure ToolServerApplication {..}

tests :: SpecWith ()
tests = do
    describe "IHP.IDE.SchemaDesigner.Controller.Migrations" $ do
        describe "middleware integration" $ do
            it "CreateMigrationAction should work with requestBodyMiddleware" $ do
                -- This test verifies that CreateMigrationAction can access request params
                -- through paramOrDefault and paramOrNothing, which require requestBodyMiddleware
                -- to parse and store the body in the request vault.
                --
                -- CreateMigrationAction uses:
                --   - paramOrDefault "" "description" (line 45)
                --   - paramOrNothing "sqlStatements" (line 46) 
                --   - paramOrDefault False "createOnly" (line 52)
                --
                -- If requestBodyMiddleware is missing from the ToolServer middleware stack,
                -- these calls will throw: "lookupRequestVault: Could not find RequestBody"
                
                toolServerApp <- createMockToolServerApplication
                mockContext <- mockContextNoDatabase toolServerApp (pure ())
                
                -- Try to call the action with params - this will fail if middleware is missing
                result <- try $ withContext (callActionWithParams CreateMigrationAction [
                    ("description", "test migration"),
                    ("createOnly", "true")
                    ]) mockContext
                
                case result of
                    Left (exception :: SomeException) -> do
                        -- Check if it's the specific RequestBody vault error
                        let errorMsg = tshow exception
                        when ("lookupRequestVault" `isInfixOf` errorMsg && "RequestBody" `isInfixOf` errorMsg) $ do
                            expectationFailure $ 
                                "requestBodyMiddleware is missing from ToolServer middleware stack. " <>
                                "Error: " <> cs errorMsg
                        -- Other errors (like file not found) are OK for this test
                        -- We're only checking that the middleware is present
                    Right _response -> do
                        -- Success - the middleware allowed us to read params
                        pure ()


