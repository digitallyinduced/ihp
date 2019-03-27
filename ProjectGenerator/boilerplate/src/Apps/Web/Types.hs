module Apps.Web.Types where
import ClassyPrelude hiding (index, delete, show, take)
import Model.Generated.Types

import           ClassyPrelude
import           Apps.Web.Controller.Context
import qualified Foundation.Controller.Session
import           Foundation.ControllerSupport  (RequestContext (RequestContext))
import qualified Foundation.ControllerSupport
import           Foundation.HaskellSupport
import           Foundation.ModelSupport
import           Helper.Controller
import           Model.Generated.Types
import qualified Network.Wai
import Data.Dynamic
import Foundation.ViewSupport
import Data.Data

data WebApplication = WebApplication deriving (Eq, Show, Generic)

data ViewContext = ViewContext
    { requestContext :: RequestContext
    , flashMessages :: [Foundation.Controller.Session.FlashMessage]
    , validations :: [Dynamic]
    , layout :: Layout
    } deriving (Generic)
