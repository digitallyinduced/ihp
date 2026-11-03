module SchemaCompiler.TestSupport
    ( Snapshot, fixtureSchema, prepareSchema, outputHashes, outputTimes
    , cacheFile, runCompiler, requireEqualOutputs
    ) where

import Prelude
import Control.Monad (forM, unless, when)
import qualified "cryptohash-sha256" Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>), makeRelative, takeExtension)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

type Snapshot = Map.Map FilePath BS.ByteString

fixtureSchema :: Text.Text
fixtureSchema = Text.unlines
    [ "CREATE TYPE moods AS ENUM ('happy', 'very happy', 'sad');"
    , "CREATE TYPE other_moods AS ENUM ('happy', 'calm');"
    , "CREATE TYPE empty_enum AS ENUM ();"
    , "CREATE TABLE companies (id UUID PRIMARY KEY, name TEXT NOT NULL UNIQUE);"
    , "CREATE TABLE users (id UUID DEFAULT uuid_generate_v4() PRIMARY KEY, company_id UUID, email TEXT NOT NULL UNIQUE, mood moods, previous_moods moods[], created_at TIMESTAMP WITH TIME ZONE NOT NULL);"
    , "ALTER TABLE users ADD CONSTRAINT users_company_fk FOREIGN KEY (company_id) REFERENCES companies (id);"
    , "CREATE TABLE referrals (id UUID PRIMARY KEY, user_id UUID NOT NULL, referred_user_id UUID);"
    , "ALTER TABLE referrals ADD CONSTRAINT referrals_user_fk FOREIGN KEY (user_id) REFERENCES users (id);"
    , "ALTER TABLE referrals ADD CONSTRAINT referrals_referred_user_fk FOREIGN KEY (referred_user_id) REFERENCES users (id);"
    , "CREATE TABLE profiles (id UUID PRIMARY KEY, user_email TEXT, other_mood other_moods);"
    , "ALTER TABLE profiles ADD CONSTRAINT profiles_email_fk FOREIGN KEY (user_email) REFERENCES users (email);"
    , "CREATE TABLE audit_events (id BIGSERIAL PRIMARY KEY, user_id UUID NOT NULL, \"raw/payload\" TEXT);"
    , "COMMENT ON TABLE audit_events IS 'ihp:no-codegen';"
    , "ALTER TABLE audit_events ADD CONSTRAINT audit_user_fk FOREIGN KEY (user_id) REFERENCES users (id);"
    , "CREATE TABLE event_links (id UUID PRIMARY KEY, audit_event_id BIGINT);"
    , "ALTER TABLE event_links ADD CONSTRAINT event_links_audit_fk FOREIGN KEY (audit_event_id) REFERENCES audit_events (id);"
    , "CREATE TABLE memberships (user_id UUID NOT NULL, company_id UUID NOT NULL, PRIMARY KEY (user_id, company_id));"
    , "ALTER TABLE memberships ADD CONSTRAINT memberships_user_fk FOREIGN KEY (user_id) REFERENCES users (id);"
    , "ALTER TABLE memberships ADD CONSTRAINT memberships_company_fk FOREIGN KEY (company_id) REFERENCES companies (id);"
    , "CREATE TABLE base_records (id SERIAL PRIMARY KEY, title TEXT NOT NULL);"
    , "CREATE TABLE child_records (id SERIAL PRIMARY KEY, extra TEXT) INHERITS (base_records);"
    , "CREATE TABLE without_primary_key (value TEXT);"
    ]

prepareSchema :: FilePath -> Text.Text -> IO ()
prepareSchema root sql = do
    createDirectoryIfMissing True (root </> "Application")
    Text.writeFile (root </> "Application/Schema.sql") sql

outputHashes :: FilePath -> IO Snapshot
outputHashes root = do
    files <- generatedFiles (root </> "build/Generated")
    entries <- forM files \path -> do
        digest <- SHA256.hash <$> BS.readFile path
        pure (makeRelative root path, digest)
    pure (Map.fromList entries)
    where
        generatedFiles directory = do
            names <- listDirectory directory
            concat <$> forM names \name -> do
                let path = directory </> name
                isDirectory <- doesDirectoryExist path
                if isDirectory
                    then generatedFiles path
                    else pure [path | takeExtension path == ".hs"]

outputTimes :: FilePath -> Snapshot -> IO (Map.Map FilePath UTCTime)
outputTimes root = Map.traverseWithKey (\path _ -> getModificationTime (root </> path))

cacheFile :: FilePath -> FilePath
cacheFile root = root </> "build/.ihp-schema-compiler-cache"

runCompiler :: FilePath -> FilePath -> Bool -> IO Double
runCompiler binary root relations = do
    environment <- getEnvironment
    let inherited = filter (\(name, _) -> name /= "GHCRTS" && name /= "IHP_RELATION_SUPPORT") environment
    let process = (proc binary [])
            { cwd = Just root
            , env = Just (("IHP_RELATION_SUPPORT", if relations then "1" else "0") : inherited)
            }
    start <- getMonotonicTimeNSec
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    end <- getMonotonicTimeNSec
    unless (exitCode == ExitSuccess) (fail (stdout <> stderr))
    pure (fromIntegral (end - start) / 1e9)

requireEqualOutputs :: Snapshot -> Snapshot -> IO ()
requireEqualOutputs expected actual = do
    when (Map.null expected) (fail "No generated files")
    unless (expected == actual) do
        let paths = Map.keysSet expected <> Map.keysSet actual
        let changed = filter (\path -> Map.lookup path expected /= Map.lookup path actual) (foldr (:) [] paths)
        fail ("Generated files differ: " <> show changed)
