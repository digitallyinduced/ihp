module IHP.AuthSupport.View.PasswordResets.Show where
import IHP.ViewPrelude
import IHP.AuthSupport.Types
import IHP.RouterSupport

data ShowView user = ShowView
    { user :: user
    , token :: Text
    }