-- | WAI middleware for early return from request handlers.
--
-- This middleware catches 'EarlyReturnException' and returns the already-sent response,
-- allowing handlers to exit early without the exception propagating further.
--
-- Usage:
--
-- > import Network.Wai.Middleware.EarlyReturn
-- >
-- > app :: Application
-- > app = earlyReturnMiddleware myApp
-- >
-- > handler :: IO ResponseReceived
-- > handler = do
-- >     when someCondition do
-- >         earlyReturn (sendResponse ...)
-- >     normalResponse
--
module Network.Wai.Middleware.EarlyReturn
    ( earlyReturn
    , earlyReturnMiddleware
    , EarlyReturnException (..)
    ) where

import Prelude
import Network.Wai (ResponseReceived, Middleware)
import qualified Control.Exception as Exception
import Control.Exception (Exception)

-- | Exit a request handler early after sending a response.
--
-- Use this when you want to conditionally exit a handler:
--
-- > handler = do
-- >     when someCondition do
-- >         earlyReturn (sendErrorResponse ...)
-- >     -- rest of the handler
-- >     sendNormalResponse
--
-- The function runs the given IO action (which should send a response),
-- then throws 'EarlyReturnException' to exit. The middleware catches this
-- exception so it doesn't propagate further.
earlyReturn :: IO ResponseReceived -> IO ()
earlyReturn action = do
    result <- action
    Exception.throwIO (EarlyReturnException result)
{-# INLINE earlyReturn #-}

-- | Middleware that catches 'EarlyReturnException'.
--
-- Apply this middleware to your application to enable 'earlyReturn':
--
-- > app = earlyReturnMiddleware myApp
--
earlyReturnMiddleware :: Middleware
earlyReturnMiddleware app request respond =
    app request respond `Exception.catch` \(EarlyReturnException r) -> pure r
{-# INLINE earlyReturnMiddleware #-}

-- | Exception thrown by 'earlyReturn' to exit a handler after the response has been sent.
newtype EarlyReturnException = EarlyReturnException ResponseReceived

instance Show EarlyReturnException where
    show _ = "EarlyReturnException"

instance Exception EarlyReturnException
