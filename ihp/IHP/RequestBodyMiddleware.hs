{-|
Module: IHP.RequestBodyMiddleware
Description: Middleware that parses the request body and stores it in the request vault
Copyright: (c) digitally induced GmbH, 2024

This middleware parses the HTTP request body (either as JSON or form data)
and stores it in the WAI request vault for later access via request.parsedBody.
-}
module IHP.RequestBodyMiddleware
( requestBodyMiddleware
  -- * RequestBody type
, RequestBody (..)
, requestBodyVaultKey
  -- * Type alias
, Respond
) where

import IHP.Prelude
import Network.Wai
import Network.HTTP.Types.Header (hContentType)
import qualified Network.Wai.Parse as WaiParse
import qualified Data.Aeson as Aeson
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
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
    let contentType = lookup hContentType (requestHeaders req)
    requestBody <- case contentType of
        Just "application/json" -> do
            rawPayload <- lazyRequestBody req
            let jsonPayload = Aeson.decode rawPayload
            pure JSONBody { jsonPayload, rawPayload }
        _ -> do
            (params, files) <- WaiParse.parseRequestBodyEx parseRequestBodyOptions WaiParse.lbsBackEnd req
            pure FormBody { params, files }

    let req' = req { vault = Vault.insert requestBodyVaultKey requestBody req.vault }
    app req' respond
