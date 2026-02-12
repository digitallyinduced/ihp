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

-- | Type alias for WAI respond function
type Respond = Response -> IO ResponseReceived

-- | Represents the parsed HTTP request body
data RequestBody
    -- | A form body with URL-encoded or multipart params and files
    = FormBody { params :: [Param], files :: [File LBS.ByteString] }
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
        then pure (FormBody [] [])
        else do
            let contentType = lookup hContentType (requestHeaders req)
            case contentType of
                Just ct | isJsonContentType ct -> do
                    rawPayload <- strictRequestBody req
                    let jsonPayload = Aeson.decode rawPayload
                    pure JSONBody { jsonPayload, rawPayload }
                _ -> do
                    (params, files) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions WaiParse.lbsBackEnd req
                    pure FormBody { params, files }

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
