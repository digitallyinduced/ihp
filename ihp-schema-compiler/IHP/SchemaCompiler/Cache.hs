-- Content-verified cache for repeated schema compilation. The Nix data path
-- identifies the immutable compiler and its dependencies, not just its version.
module IHP.SchemaCompiler.Cache
    ( Entry, entry, cacheKey, isFresh, store ) where

import Prelude
import Control.Exception (bracketOnError)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory (removeFile, renameFile)
import System.FilePath (isRelative, normalise, splitDirectories, takeExtension)
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (tryIOError)

-- The digest covers the complete manifest, so truncation cannot look like a
-- shorter, valid list of outputs. Cache corruption always causes regeneration.
data Entry = Entry !FilePath !BS.ByteString

cacheFile :: FilePath
cacheFile = "build/.ihp-schema-compiler-cache"

magic :: BS.ByteString
magic = "IHP schema compiler cache v1\0"

cacheKey :: FilePath -> Bool -> Text -> BS.ByteString
cacheKey compilerIdentity relations sql = SHA256.hash
    (Text.encodeUtf8 (Text.pack compilerIdentity) <> "\0"
        <> (if relations then "1" else "0") <> "\0" <> Text.encodeUtf8 sql)

entry :: FilePath -> BS.ByteString -> Entry
entry path content = Entry path (SHA256.hash content)

isFresh :: BS.ByteString -> IO Bool
isFresh key = do
    cached <- tryIOError (BS.readFile cacheFile)
    case either (const Nothing) (decode key) cached of
        Nothing -> pure False
        Just entries -> verify entries
    where
        verify [] = pure True
        verify (Entry path expected : remaining) = do
            content <- tryIOError (BS.readFile path)
            case content of
                Right bytes | SHA256.hash bytes == expected -> verify remaining
                _ -> pure False

decode :: BS.ByteString -> BS.ByteString -> Maybe [Entry]
decode key bytes = do
    payload <- BS.stripPrefix (magic <> key) bytes
    let (expectedDigest, body) = BS.splitAt 32 payload
    if BS.length expectedDigest /= 32 || SHA256.hash body /= expectedDigest
        then Nothing
        else do
            entries <- decodeEntries body
            if null entries then Nothing else Just entries
    where
        decodeEntries bytes
            | BS.null bytes = Just []
            | otherwise = do
                let (pathBytes, rest) = BS.break (== 0) bytes
                path <- either (const Nothing) (Just . Text.unpack) (Text.decodeUtf8' pathBytes)
                let (digest, remaining) = BS.splitAt 32 (BS.drop 1 rest)
                if BS.null rest || BS.length digest /= 32 || not (validOutputPath path)
                    then Nothing
                    else (Entry path digest :) <$> decodeEntries remaining

-- Cache entries are always relative Haskell files under the generated tree.
validOutputPath :: FilePath -> Bool
validOutputPath path = isRelative path && normalise path == path
    && takeExtension path == ".hs" && ".." `notElem` splitDirectories path
    && case splitDirectories path of
        "build" : "Generated" : _ : _ -> True
        _ -> False

store :: BS.ByteString -> [Entry] -> IO ()
store key entries = do
    let body = BS.concat [Text.encodeUtf8 (Text.pack path) <> "\0" <> digest | Entry path digest <- entries]
    let bytes = magic <> key <> SHA256.hash body <> body
    -- A cache is optional; a failed cache write must not fail a successful
    -- generation. Replace it atomically, and preserve asynchronous exceptions.
    _ <- tryIOError $ bracketOnError
        (openBinaryTempFile "build" ".ihp-schema-compiler-cache")
        cleanup
        (\(path, handle) -> do
            BS.hPut handle bytes
            hClose handle
            renameFile path cacheFile)
    pure ()
    where
        cleanup (path, handle) = do
            _ <- tryIOError (hClose handle)
            _ <- tryIOError (removeFile path)
            pure ()
