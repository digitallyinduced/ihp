module IHP.Controller.Response
( respondWith
, addResponseHeaders
, addResponseHeadersFromContext
-- Re-exported from Network.Wai.Middleware.EarlyReturn
, earlyReturn
, EarlyReturnException (..)
, ResponseException (..)
, responseHeadersVaultKey
)
where

import ClassyPrelude
import Network.HTTP.Types.Header
import qualified Network.Wai
import Network.Wai (Response, Request, ResponseReceived)
import Wai.Request.Params.Middleware (Respond)
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import IHP.RequestVault.Helper (lookupRequestVault)
import Network.Wai.Middleware.EarlyReturn (earlyReturn, EarlyReturnException(..))
import qualified Control.Exception as Exception

-- | Sends a response to the client. Used by render functions.
--
-- This is the normal way to respond - it calls the WAI respond callback directly
-- and returns the ResponseReceived.
respondWith :: (?request :: Request, ?respond :: Respond) => Response -> IO ResponseReceived
respondWith response = do
    responseWithHeaders <- addResponseHeadersFromContext response
    ?respond responseWithHeaders
{-# INLINE respondWith #-}

-- | Add headers to current response
-- | Returns a Response with headers
--
-- > addResponseHeaders [("Content-Type", "text/html")] response
--
addResponseHeaders :: [Header] -> Response -> Response
addResponseHeaders headers = Network.Wai.mapResponseHeaders (\hs -> headers <> hs)
{-# INLINE addResponseHeaders #-}

-- | Add headers to current response, getting the headers from the request vault
-- | Returns a Response with headers
--
-- > addResponseHeadersFromContext response
-- You probabaly want `setHeader`
--
addResponseHeadersFromContext :: (?request :: Request) => Response -> IO Response
addResponseHeadersFromContext response = do
    headers <- readIORef (lookupRequestVault responseHeadersVaultKey ?request)
    let responseWithHeaders = addResponseHeaders headers response
    pure responseWithHeaders
{-# INLINE addResponseHeadersFromContext #-}

responseHeadersVaultKey :: Vault.Key (IORef [Header])
responseHeadersVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE responseHeadersVaultKey #-}

-- | Can be thrown from inside the action to abort the current action execution.
-- This is kept for backwards compatibility with code that throws response exceptions
-- (e.g. requestBodyJSON). Prefer using 'earlyReturn' for new code.
newtype ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException
