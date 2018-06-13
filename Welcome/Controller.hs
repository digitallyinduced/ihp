module Foundation.Welcome.Controller where

import Foundation.ControllerPrelude

welcome :: Action
welcome = do
    renderPlain "It's working! :)"
