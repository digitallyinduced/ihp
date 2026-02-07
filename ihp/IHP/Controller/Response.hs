module IHP.Controller.Response
( respondAndExit
, respondAndExitWithHeaders
, addResponseHeaders
, addResponseHeadersFromContext
, ResponseException (..)
, responseHeadersVaultKey
)
where

import ClassyPrelude
import Network.HTTP.Types.Header
import qualified Network.Wai
import Network.Wai (Response, Request)
import qualified Control.Exception as Exception
import qualified Data.Vault.Lazy as Vault
import System.IO.Unsafe (unsafePerformIO)
import IHP.RequestVault.Helper (lookupRequestVault)

-- | Simple version - just throws the response, no context needed
respondAndExit :: Response -> IO ()
respondAndExit response = Exception.throwIO (ResponseException response)
{-# INLINE respondAndExit #-}

-- | Version that adds headers from context (for render, etc.)
respondAndExitWithHeaders :: (?request :: Request) => Response -> IO ()
respondAndExitWithHeaders response = do
    responseWithHeaders <- addResponseHeadersFromContext response
    Exception.throwIO (ResponseException responseWithHeaders)
{-# INLINE respondAndExitWithHeaders #-}

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

-- Can be thrown from inside the action to abort the current action execution.
-- Does not indicates a runtime error. It's just used for control flow management.
newtype ResponseException = ResponseException Response

instance Show ResponseException where show _ = "ResponseException { .. }"

instance Exception ResponseException

responseHeadersVaultKey :: Vault.Key (IORef [Header])
responseHeadersVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE responseHeadersVaultKey #-}