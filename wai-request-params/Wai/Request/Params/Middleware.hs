{-|
Module: Wai.Request.Params.Middleware
Description: Middleware that parses the request body and stores it in the request vault
Copyright: (c) digitally induced GmbH, 2024

This middleware parses the HTTP request body (either as JSON or form data)
and stores it in the WAI request vault for later access via request.parsedBody.
-}
module Wai.Request.Params.Middleware
( requestBodyMiddleware
  -- * RequestBody type
, RequestBody (..)
, requestBodyVaultKey
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
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Parse (File, Param)
import Data.IORef (newIORef, atomicModifyIORef')

-- | Type alias for WAI respond function
type Respond = Response -> IO ResponseReceived

-- | Represents the parsed HTTP request body
data RequestBody
    -- | A form body with URL-encoded or multipart params and files
    = FormBody { params :: [Param], files :: [File LBS.ByteString], rawPayload :: LBS.ByteString }
    -- | A JSON body
    | JSONBody { jsonPayload :: Maybe Aeson.Value, rawPayload :: LBS.ByteString }

-- | Vault key for storing the parsed request body
requestBodyVaultKey :: Vault.Key RequestBody
requestBodyVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE requestBodyVaultKey #-}

-- | Middleware that parses the request body and stores it in the request vault.
--
-- Takes 'parseRequestBodyOptions' as an explicit parameter to avoid depending
-- on FrameworkConfig being in the vault.
--
-- After this middleware runs, you can access the parsed body via:
--
-- > request.parsedBody
--
requestBodyMiddleware :: WaiParse.ParseRequestBodyOptions -> Middleware
requestBodyMiddleware parseRequestBodyOptions app req respond = do
    let method = requestMethod req
    requestBody <- if method == methodGet || method == methodHead || method == methodDelete || method == methodOptions
        then pure (FormBody [] [] LBS.empty)
        else do
            -- Always read the raw body first so it's available via getRequestBody
            rawPayload <- strictRequestBody req
            let contentType = lookup hContentType (requestHeaders req)
            case contentType of
                Just ct | isJsonContentType ct -> do
                    let jsonPayload = Aeson.decode rawPayload
                    pure JSONBody { jsonPayload, rawPayload }
                _ -> do
                    -- WAI's request body is a stream that can only be read
                    -- once — each getRequestBodyChunk call pops the next
                    -- chunk and removes it. Since strictRequestBody above
                    -- already consumed the stream, we create a new body
                    -- reader backed by an IORef holding the already-read
                    -- chunks. Each time the form parser calls
                    -- getRequestBodyChunk, it pops the next chunk from the
                    -- IORef, effectively "replaying" the original bytes.
                    ref <- newIORef (LBS.toChunks rawPayload)
                    let bodyReader = atomicModifyIORef' ref $ \chunks -> case chunks of
                            [] -> ([], BS.empty)
                            (c:cs) -> (cs, c)
                    let req' = setRequestBodyChunks bodyReader req
                    (params, files) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions WaiParse.lbsBackEnd req'
                    pure FormBody { params, files, rawPayload }

    let req' = req { vault = Vault.insert requestBodyVaultKey requestBody (vault req) }
    app req' respond

-- | Checks if a Content-Type header value indicates JSON.
--
-- Matches @application/json@ exactly, or @application/json@ followed by
-- a semicolon and parameters (e.g. @application/json; charset=utf-8@).
isJsonContentType :: BS.ByteString -> Bool
isJsonContentType ct =
    ct == BS.pack "application/json"
    || BS.pack "application/json;" `BS.isPrefixOf` ct
