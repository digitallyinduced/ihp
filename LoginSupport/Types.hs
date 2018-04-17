module Foundation.LoginSupport.Types (throwNotLoggedIn, NotLoggedIn (NotLoggedIn)) where

import           ClassyPrelude hiding (throw)
import           Control.Exception             (throw)

data NotLoggedIn = NotLoggedIn { newSessionUrl :: Maybe Text } deriving Show

throwNotLoggedIn newSessionUrl = throw (Foundation.LoginSupport.Types.NotLoggedIn newSessionUrl)

instance Exception NotLoggedIn