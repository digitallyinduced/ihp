module IHP.AuthSupport.View.Sessions.New where
import Web.View.Prelude
import IHP.LoginSupport.Helper.View

data NewView user = NewView { user :: user } deriving (Typeable)