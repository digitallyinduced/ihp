module Apps.Web.Types where
import Apps.Web.Validation

import           ClassyPrelude
import           Apps.Web.Controller.Context
import qualified Foundation.Controller.Session
import qualified Foundation.ControllerSupport as ControllerSupport
import           Foundation.HaskellSupport
import           Foundation.ModelSupport
import           Helper.Controller
import qualified Network.Wai
import Foundation.ViewSupport
import Model.Generated.Types
import Data.Dynamic
import Data.Data

data WebApplication = WebApplication deriving (Eq, Show, Generic)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [Foundation.Controller.Session.FlashMessage]
    , validations :: [Dynamic]
    , layout :: Layout
    } deriving (Generic)
