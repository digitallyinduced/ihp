module IHP.AuthSupport.View.PasswordResets.New where
import IHP.ViewPrelude
import IHP.AuthSupport.Types

data NewView user = NewView { passwordReset :: PasswordReset }
