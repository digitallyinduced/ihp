module IHP.AuthSupport.View.Sessions.New where
import IHP.Prelude

data NewView user = NewView { user :: user } deriving (Typeable)