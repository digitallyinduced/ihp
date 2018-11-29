module Apps.Web.Controller.Context where

import ClassyPrelude
import Foundation.Controller.Session
import Foundation.Controller.RequestContext
import Foundation.ModelSupport
import Model.Generated.Types
import Data.Dynamic

data ControllerContext = ControllerContext {
        -- Here you can prepare data to be available in your controller actions
        -- E.g. you might want to fetch the current logged in user here
        -- user :: Maybe User
        validations :: IORef [Dynamic]
    } deriving (Generic)

createControllerContext :: (?requestContext :: RequestContext, ?modelContext :: ModelContext) => IO ControllerContext
createControllerContext = do
	validations <- newIORef []
    return $ ControllerContext { validations }
