module IHP.Controller.Response
( respondWith
, respondAndExit
, addResponseHeaders
, addResponseHeadersFromContext
-- Re-exported from Network.Wai.Middleware.EarlyReturn
, earlyReturn
, EarlyReturnException (..)
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

-- | Sends a response and exits the current action via early return.
-- Sends the response via 'respondWith' then throws 'EarlyReturnException'
-- so the action short-circuits.
respondAndExit :: (?request :: Request, ?respond :: Respond) => Response -> IO a
respondAndExit response = earlyReturn (respondWith response)
{-# INLINE respondAndExit #-}
