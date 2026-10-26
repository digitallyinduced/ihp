module SchemaCompiler.CacheSpec (tests) where

import Prelude
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified IHP.SchemaCompiler as Compiler
import SchemaCompiler.TestSupport
import System.Directory (createDirectory, getModificationTime, removeFile, setModificationTime, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

data Fixture = Fixture FilePath Snapshot

tests :: Spec
tests = describe "Schema compiler cache" do
    forM_ [False, True] \relations ->
        describe ("relation support " <> show relations) $ around (withFixture relations) do
            it "reuses the cache without rewriting any generated file" \(Fixture root expected) -> do
                times <- outputTimes root expected
                let sentinel = UTCTime (fromGregorian 2000 1 1) 0
                setModificationTime (cacheFile root) sentinel
                compileIn root relations
                outputHashes root `shouldReturn` expected
                outputTimes root expected `shouldReturn` times
                getModificationTime (cacheFile root) `shouldReturn` sentinel

            it "detects changed contents even when size and mtime are preserved" \(Fixture root expected) -> do
                path <- firstOutput root expected
                timestamp <- getModificationTime path
                content <- BS.readFile path
                case BS.uncons content of
                    Nothing -> expectationFailure "Generated file is empty"
                    Just (first, remaining) -> BS.writeFile path (BS.cons (first `xor` 1) remaining)
                setModificationTime path timestamp
                compileIn root relations
                outputHashes root `shouldReturn` expected

            it "restores deleted outputs" \(Fixture root expected) -> do
                path <- firstOutput root expected
                removeFile path
                compileIn root relations
                outputHashes root `shouldReturn` expected

            it "repairs an invalid manifest" \(Fixture root expected) -> do
                manifest <- BS.readFile (cacheFile root)
                BS.writeFile (cacheFile root) "invalid cache"
                compileIn root relations
                outputHashes root `shouldReturn` expected
                BS.readFile (cacheFile root) `shouldReturn` manifest

            it "repairs a truncated manifest" \(Fixture root expected) -> do
                manifest <- BS.readFile (cacheFile root)
                BS.writeFile (cacheFile root) (BS.take (BS.length manifest - 1) manifest)
                compileIn root relations
                outputHashes root `shouldReturn` expected
                BS.readFile (cacheFile root) `shouldReturn` manifest

            it "invalidates after schema changes" \(Fixture root original) -> do
                let sql = fixtureSchema <> "\nCREATE TABLE cache_tests (id SERIAL PRIMARY KEY, name TEXT);\n"
                expected <- freshOutputs sql relations
                expected `shouldNotBe` original
                prepareSchema root sql
                compileIn root relations
                outputHashes root `shouldReturn` expected

            it "invalidates after relation options change" \(Fixture root original) -> do
                refreshed <- freshOutputs fixtureSchema (not relations)
                -- Generation preserves existing files that the new mode does
                -- not emit, such as Include modules when relations are disabled.
                let expected = Map.union refreshed original
                expected `shouldNotBe` original
                compileIn root (not relations)
                outputHashes root `shouldReturn` expected

            it "still generates correctly when the optional cache cannot be written" \(Fixture root expected) -> do
                removeFile (cacheFile root)
                createDirectory (cacheFile root)
                compileIn root relations
                outputHashes root `shouldReturn` expected

withFixture :: Bool -> (Fixture -> IO ()) -> IO ()
withFixture relations action = withSystemTempDirectory "ihp-schema-cache-" \root -> do
    prepareSchema root fixtureSchema
    compileIn root relations
    expected <- outputHashes root
    expected `shouldSatisfy` (not . Map.null)
    action (Fixture root expected)

freshOutputs :: Text.Text -> Bool -> IO Snapshot
freshOutputs sql relations = withSystemTempDirectory "ihp-schema-reference-" \root -> do
    prepareSchema root sql
    compileIn root relations
    outputHashes root

compileIn :: FilePath -> Bool -> IO ()
compileIn root relations = bracket (lookupEnv "IHP_RELATION_SUPPORT") restore \_ -> do
    setEnv "IHP_RELATION_SUPPORT" (if relations then "1" else "0")
    withCurrentDirectory root Compiler.compile
    where
        restore = maybe (unsetEnv "IHP_RELATION_SUPPORT") (setEnv "IHP_RELATION_SUPPORT")

firstOutput :: FilePath -> Snapshot -> IO FilePath
firstOutput root snapshot = case Map.lookupMin snapshot of
    Nothing -> fail "No generated files"
    Just (path, _) -> pure (root </> path)
