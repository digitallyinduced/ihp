module Apps.Web.Controller.Context where

import ClassyPrelude hiding (pack)
import TurboHaskell.Controller.Session
import TurboHaskell.Controller.RequestContext
import TurboHaskell.ModelSupport
import Model.Generated.Types
import Data.Dynamic
import TurboHaskell.Controller.Context
import qualified Control.Newtype.Generics as Newtype

data ControllerContext = ControllerContext {
        -- Here you can prepare data to be available in your controller actions
        -- E.g. you might want to fetch the current logged in user here
        -- user :: Maybe User
        validations :: IORef [Dynamic]
    } deriving (Generic)

instance Context ControllerContext where
    createContext = do
        validations <- newIORef []
        return ControllerContext { .. }

