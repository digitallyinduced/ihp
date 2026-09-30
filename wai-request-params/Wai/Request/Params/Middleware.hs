{-|
Module: Wai.Request.Params.Middleware
Description: Middleware that parses the request body and stores it in the request vault
Copyright: (c) digitally induced GmbH, 2024

This middleware parses the HTTP request body (either as JSON or form data)
and stores it in the WAI request vault for later access via request.parsedBody.

Only bodies that need to be parsed are read eagerly:

- @application/json@ and @application/x-www-form-urlencoded@ bodies are read
  into memory and parsed.
- @multipart/form-data@ bodies are parsed straight from the request stream.
  Uploaded files are kept in memory or written to temporary files, depending
  on the 'FileUploadBackend'.
- Any other body (e.g. @application/octet-stream@ or @video/mp4@) is left
  untouched, so the application can stream it with 'getRequestBodyChunk'.
  'readRawRequestBody' reads it on first use.

'requestBodyMiddlewareDeferringMultipart' leaves @multipart/form-data@ bodies
unparsed too, so the application can choose the 'FileUploadBackend' per request
(e.g. after routing) and parse the body with 'withMultipartBody'.
-}
module Wai.Request.Params.Middleware
( requestBodyMiddleware
, requestBodyMiddlewareWith
, requestBodyMiddlewareDeferringMultipart
, withMultipartBody
  -- * RequestBody type
, RequestBody (..)
, requestBodyVaultKey
  -- * Raw request body
, readRawRequestBody
, rawRequestBodyVaultKey
  -- * File upload backend
, FileUploadBackend (..)
, tempFilesVaultKey
  -- * Type alias
, Respond
) where

import Prelude
import Network.Wai
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodGet, methodHead, methodDelete, methodOptions)
import qualified Network.Wai.Parse as WaiParse
import qualified Data.Aeson as Aeson
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Parse (File, FileInfo (..), Param)
import Data.IORef (newIORef, atomicModifyIORef')
import Control.Concurrent.MVar (newMVar, modifyMVar)
import Control.Monad.Trans.Resource (createInternalState, closeInternalState)
import Control.Exception (bracket)

-- | Type alias for WAI respond function
type Respond = Response -> IO ResponseReceived

-- | Represents the parsed HTTP request body
data RequestBody
    -- | A form body with URL-encoded or multipart params and files
    --
    -- 'rawPayload' is only filled for URL-encoded bodies. It is empty for
    -- multipart bodies (they are parsed straight from the request stream)
    -- and for bodies with any other content type (they are not read at all,
    -- use 'readRawRequestBody').
    = FormBody { params :: [Param], files :: [File LBS.ByteString], rawPayload :: LBS.ByteString }
    -- | A JSON body
    | JSONBody { jsonPayload :: Maybe Aeson.Value, rawPayload :: LBS.ByteString }

-- | Where the files of a @multipart/form-data@ body are stored while the
-- request is handled.
data FileUploadBackend
    -- | Uploaded files are kept in memory ('WaiParse.lbsBackEnd'). This is the default.
    = InMemoryFileUploads
    -- | Uploaded files are written to temporary files ('WaiParse.tempFileBackEnd').
    -- The files are removed when the request ends.
    --
    -- Their paths are stored under 'tempFilesVaultKey'. The @files@ of the
    -- 'FormBody' still work: their 'fileContent' reads the temporary file
    -- lazily, on first use, so it is only valid while the request is handled.
    | TempFileUploads
    deriving (Eq, Show)

-- | Vault key for storing the parsed request body
requestBodyVaultKey :: Vault.Key RequestBody
requestBodyVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE requestBodyVaultKey #-}

-- | Vault key for an action that returns the raw request body. Use 'readRawRequestBody'
-- instead of reading this directly.
rawRequestBodyVaultKey :: Vault.Key (IO LBS.ByteString)
rawRequestBodyVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE rawRequestBodyVaultKey #-}

-- | Set by 'requestBodyMiddlewareDeferringMultipart' when a multipart body is
-- waiting to be parsed by 'withMultipartBody'.
pendingMultipartVaultKey :: Vault.Key WaiParse.ParseRequestBodyOptions
pendingMultipartVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE pendingMultipartVaultKey #-}

-- | Vault key for the uploaded files of a multipart request, stored as
-- temporary files. Only set when using 'TempFileUploads'.
tempFilesVaultKey :: Vault.Key [File FilePath]
tempFilesVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE tempFilesVaultKey #-}

-- | Middleware that parses the request body and stores it in the request vault.
--
-- Uploaded files are kept in memory. See 'requestBodyMiddlewareWith' to store
-- them in temporary files instead.
--
-- Takes 'parseRequestBodyOptions' as an explicit parameter to avoid depending
-- on FrameworkConfig being in the vault.
--
-- After this middleware runs, you can access the parsed body via:
--
-- > request.parsedBody
--
requestBodyMiddleware :: WaiParse.ParseRequestBodyOptions -> Middleware
requestBodyMiddleware = requestBodyMiddlewareWith InMemoryFileUploads

-- | Like 'requestBodyMiddleware', but lets you choose where uploaded files are stored.
--
-- The 'WaiParse.ParseRequestBodyOptions' limits (e.g. 'WaiParse.setMaxRequestFileSize')
-- are applied while the multipart body is streamed.
requestBodyMiddlewareWith :: FileUploadBackend -> WaiParse.ParseRequestBodyOptions -> Middleware
requestBodyMiddlewareWith fileUploadBackend = requestBodyMiddlewareWithMode (ParseMultipartWith fileUploadBackend)

-- | Like 'requestBodyMiddleware', but does not parse @multipart/form-data@ bodies.
--
-- Until 'withMultipartBody' is called, a multipart request has an empty
-- 'FormBody' and its body is left unread. This allows choosing the
-- 'FileUploadBackend' per request, e.g. after routing, when it is known which
-- handler will process the upload. IHP uses this to let each controller action
-- choose its backend.
--
-- All other bodies are handled like in 'requestBodyMiddleware'.
requestBodyMiddlewareDeferringMultipart :: WaiParse.ParseRequestBodyOptions -> Middleware
requestBodyMiddlewareDeferringMultipart = requestBodyMiddlewareWithMode DeferMultipart

-- | Parses a multipart body left unparsed by 'requestBodyMiddlewareDeferringMultipart'
-- with the given backend, and calls the continuation with a request that has the
-- parsed body in its vault.
--
-- With 'TempFileUploads' the temporary files are removed when the continuation returns,
-- so the whole handling of the request should happen inside it.
--
-- When there's no pending multipart body (e.g. for JSON bodies, or when it was
-- already parsed), the continuation is called with the request unchanged.
withMultipartBody :: FileUploadBackend -> Request -> (Request -> IO a) -> IO a
withMultipartBody fileUploadBackend req continue =
    case Vault.lookup pendingMultipartVaultKey (vault req) of
        Nothing -> continue req
        Just parseRequestBodyOptions ->
            parseMultipartBody fileUploadBackend parseRequestBodyOptions req $ \requestBody extraVault ->
                continue req
                    { vault = extraVault
                        . Vault.insert requestBodyVaultKey requestBody
                        . Vault.delete pendingMultipartVaultKey
                        $ vault req
                    }

-- | How 'requestBodyMiddlewareWithMode' handles @multipart/form-data@ bodies
data MultipartMode
    = ParseMultipartWith FileUploadBackend
    | DeferMultipart

requestBodyMiddlewareWithMode :: MultipartMode -> WaiParse.ParseRequestBodyOptions -> Middleware
requestBodyMiddlewareWithMode multipartMode parseRequestBodyOptions app req respond = do
    let method = requestMethod req
    let runApp requestBody readRaw extraVault = do
            let vault' = extraVault
                    . Vault.insert rawRequestBodyVaultKey readRaw
                    . Vault.insert requestBodyVaultKey requestBody
                    $ vault req
            app req { vault = vault' } respond
    if method == methodGet || method == methodHead || method == methodDelete || method == methodOptions
        then runApp (FormBody [] [] LBS.empty) (pure LBS.empty) id
        else do
            let contentType = lookup hContentType (requestHeaders req)
            case contentType of
                Just ct | isJsonContentType ct -> do
                    rawPayload <- strictRequestBody req
                    let jsonPayload = Aeson.decode rawPayload
                    runApp JSONBody { jsonPayload, rawPayload } (pure rawPayload) id
                _ -> case WaiParse.getRequestBodyType req of
                    Just WaiParse.UrlEncoded -> do
                        -- Read the raw body first so it's available via getRequestBody
                        rawPayload <- strictRequestBody req
                        -- WAI's request body is a stream that can only be read
                        -- once — each getRequestBodyChunk call pops the next
                        -- chunk and removes it. Since strictRequestBody above
                        -- already consumed the stream, we create a new body
                        -- reader backed by an IORef holding the already-read
                        -- chunks. Each time the form parser calls
                        -- getRequestBodyChunk, it pops the next chunk from the
                        -- IORef, effectively "replaying" the original bytes.
                        -- This is the standard WAI pattern for replaying a
                        -- consumed request body, see:
                        -- https://discourse.haskell.org/t/how-to-rebuild-the-request-body-after-reading-it-in-wai-middleware/13150
                        ref <- newIORef (LBS.toChunks rawPayload)
                        let bodyReader = atomicModifyIORef' ref $ \chunks -> case chunks of
                                [] -> ([], BS.empty)
                                (c:cs) -> (cs, c)
                        let req' = setRequestBodyChunks bodyReader req
                        (params, files) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions WaiParse.lbsBackEnd req'
                        runApp FormBody { params, files, rawPayload } (pure rawPayload) id
                    Just (WaiParse.Multipart _) -> case multipartMode of
                        ParseMultipartWith fileUploadBackend ->
                            parseMultipartBody fileUploadBackend parseRequestBodyOptions req $ \requestBody extraVault ->
                                runApp requestBody (pure LBS.empty) extraVault
                        DeferMultipart ->
                            runApp (FormBody [] [] LBS.empty) (pure LBS.empty) (Vault.insert pendingMultipartVaultKey parseRequestBodyOptions)
                    Nothing -> do
                        -- Unknown content type (e.g. application/octet-stream): leave the body
                        -- unread, so the app can stream it with getRequestBodyChunk. The raw
                        -- body is only read when 'readRawRequestBody' is called.
                        readRaw <- memoize (strictRequestBody req)
                        runApp (FormBody [] [] LBS.empty) readRaw id

-- | Parses a multipart body straight from the request stream, so the body is
-- never held in memory as a whole. The continuation gets the parsed body and
-- a function to add backend-specific entries to the vault.
parseMultipartBody :: FileUploadBackend -> WaiParse.ParseRequestBodyOptions -> Request -> (RequestBody -> (Vault.Vault -> Vault.Vault) -> IO a) -> IO a
parseMultipartBody fileUploadBackend parseRequestBodyOptions req continue =
    case fileUploadBackend of
        InMemoryFileUploads -> do
            (params, files) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions WaiParse.lbsBackEnd req
            continue FormBody { params, files, rawPayload = LBS.empty } id
        TempFileUploads ->
            -- The temporary files are removed when the internal state is closed,
            -- i.e. after the continuation has returned
            bracket createInternalState closeInternalState $ \internalState -> do
                (params, tempFiles) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions (WaiParse.tempFileBackEnd internalState) req
                files <- mapM readTempFileLazily tempFiles
                continue FormBody { params, files, rawPayload = LBS.empty } (Vault.insert tempFilesVaultKey tempFiles)

-- | Returns the raw request body.
--
-- For JSON and URL-encoded bodies this is the body read by 'requestBodyMiddleware'.
-- For bodies with any other content type, the body is read from the request
-- stream on first use and cached, so calling this multiple times is fine.
-- If the application already read some chunks with 'getRequestBodyChunk', only
-- the rest of the body is returned.
--
-- For multipart bodies this returns an empty ByteString, as the body is parsed
-- straight from the request stream.
readRawRequestBody :: Request -> IO LBS.ByteString
readRawRequestBody req =
    case Vault.lookup rawRequestBodyVaultKey (vault req) of
        Just readRaw -> readRaw
        Nothing -> case Vault.lookup requestBodyVaultKey (vault req) of
            Just requestBody -> pure (rawPayload requestBody)
            Nothing -> pure LBS.empty

-- | Runs the action on first use and returns the cached result afterwards.
memoize :: IO a -> IO (IO a)
memoize action = do
    cache <- newMVar Nothing
    pure $ modifyMVar cache $ \case
        Just value -> pure (Just value, value)
        Nothing -> do
            value <- action
            pure (Just value, value)

-- | Turns an uploaded temporary file into one whose content is read from disk on first use.
readTempFileLazily :: File FilePath -> IO (File LBS.ByteString)
readTempFileLazily (name, fileInfo) = do
    content <- unsafeInterleaveIO (LBS.readFile (fileContent fileInfo))
    pure (name, fileInfo { fileContent = content })

-- | Checks if a Content-Type header value indicates JSON.
--
-- Matches @application/json@ exactly, or @application/json@ followed by
-- a semicolon and parameters (e.g. @application/json; charset=utf-8@).
isJsonContentType :: BS.ByteString -> Bool
isJsonContentType ct =
    ct == BS.pack "application/json"
    || BS.pack "application/json;" `BS.isPrefixOf` ct
