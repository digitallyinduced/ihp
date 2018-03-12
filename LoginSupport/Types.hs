module Foundation.LoginSupport.Types (throwNotLoggedIn, NotLoggedIn) where

import           ClassyPrelude hiding (throw)
import           Control.Exception             (throw)

data NotLoggedIn = NotLoggedIn deriving Show

throwNotLoggedIn = throw Foundation.LoginSupport.Types.NotLoggedIn

instance Exception NotLoggedIn