module TurboHaskell.Welcome.Controller where

import TurboHaskell.ControllerPrelude
import TurboHaskell.RouterPrelude

data WelcomeController = WelcomeAction

instance CanRoute WelcomeController () where
    parseRoute = runAction <$> parseRoute' @WelcomeController
    parseRoute' = (string "/" <|> string "") *> endOfInput *> return WelcomeAction

instance HasPath WelcomeController where
    pathTo WelcomeAction = "/"

instance Controller WelcomeController () where
    action WelcomeAction = renderPlain "It's working! :)"
