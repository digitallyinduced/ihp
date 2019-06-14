module Apps.Web.Types where
import Apps.Web.Validation

import           ClassyPrelude
import           Apps.Web.Controller.Context
import qualified TurboHaskell.Controller.Session
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import           TurboHaskell.HaskellSupport
import           TurboHaskell.ModelSupport
import           Helper.Controller
import qualified Network.Wai
import TurboHaskell.ViewSupport
import Model.Generated.Types
import Data.Dynamic
import Data.Data

data WebApplication = WebApplication deriving (Eq, Show, Generic)

data ViewContext = ViewContext
    { requestContext :: ControllerSupport.RequestContext
    , flashMessages :: [TurboHaskell.Controller.Session.FlashMessage]
    , validations :: [Dynamic]
    , layout :: Layout
    } deriving (Generic)
