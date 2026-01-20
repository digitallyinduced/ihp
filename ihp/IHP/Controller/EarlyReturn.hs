-- | Provides early return functionality for controller actions.
--
-- When you need to conditionally exit an action early (like for access control),
-- use 'earlyReturn' to send a response and stop execution:
--
-- > action MyAction = do
-- >     when (not loggedIn) do
-- >         earlyReturn (redirectTo LoginAction)
-- >     render MyView
--
module IHP.Controller.EarlyReturn
( earlyReturn
, handleEarlyReturn
, EarlyReturnException (..)
)
where

import Prelude
import Network.Wai (ResponseReceived)
import qualified Control.Exception as Exception
import Control.Exception (Exception)

-- | Exit the action early by running another action that returns a response.
--
-- This is useful when you want to conditionally exit an action:
--
-- > action MyAction = do
-- >     when someCondition do
-- >         earlyReturn $ redirectTo SomeOtherAction
-- >     -- rest of the action
-- >     render MyView
--
-- The 'earlyReturn' function takes an IO action that returns 'ResponseReceived'
-- (like 'render' or 'redirectTo'), runs it, and then throws an exception to
-- exit early. The response is still sent to the client.
earlyReturn :: IO ResponseReceived -> IO ()
earlyReturn action = do
    result <- action
    Exception.throwIO (EarlyReturnException result)
{-# INLINE earlyReturn #-}

-- | Run an action and catch any 'EarlyReturnException', returning the 'ResponseReceived'.
--
-- This is useful when you need to run an action that might use 'earlyReturn' in a context
-- where exceptions shouldn't propagate (like AutoRefresh).
--
-- > result <- handleEarlyReturn (action controller)
--
handleEarlyReturn :: IO ResponseReceived -> IO ResponseReceived
handleEarlyReturn action = action `Exception.catch` \(EarlyReturnException response) -> pure response
{-# INLINE handleEarlyReturn #-}

-- | Exception thrown by 'earlyReturn' to exit the action after the response has been sent.
newtype EarlyReturnException = EarlyReturnException ResponseReceived

instance Show EarlyReturnException where show _ = "EarlyReturnException"

instance Exception EarlyReturnException
