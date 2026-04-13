{-|
Module: IHP.Controller.Context
Copyright: (c) digitally induced GmbH, 2020

@'ControllerContext'@ is a type alias for the WAI 'Request' — all
request-scoped state lives in @request.vault@ (see 'IHP.RequestVault').
The alias is preserved so existing code that uses the @?context@ implicit
parameter keeps working; 'HasField' instances for @frameworkConfig@ and
@logger@ come from 'IHP.RequestVault'.
-}
module IHP.Controller.Context
    ( ControllerContext
    , newControllerContext
    , ActionType(..)
    ) where

import Prelude
import Network.Wai (Request)
import IHP.ActionType (ActionType(..))
import IHP.RequestVault () -- for HasField "frameworkConfig"/"logger"/"pgListener" on Request

-- | The WAI 'Request' threaded through controllers and views.
type ControllerContext = Request

-- | Returns the current request. Kept for source compatibility with callers
-- that previously wrapped the request in a @ControllerContext@.
newControllerContext :: (?request :: Request) => IO ControllerContext
newControllerContext = pure ?request
{-# INLINE newControllerContext #-}
