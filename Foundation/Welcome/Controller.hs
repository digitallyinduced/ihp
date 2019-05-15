module Foundation.Welcome.Controller where

import Foundation.ControllerPrelude
import Foundation.RouterPrelude

data WelcomeController = WelcomeAction

instance CanRoute WelcomeController () where
	parseRoute = parseRoute' @WelcomeController >>= return . runAction
	parseRoute' = (string "/" <|> string "") *> endOfInput *> return WelcomeAction

instance HasPath WelcomeController where
	pathTo WelcomeAction = "/"

instance Controller WelcomeController () where
	action WelcomeAction = renderPlain "It's working! :)"
