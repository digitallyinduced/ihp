module Web.FrontController where

import IHP.Prelude
import IHP.ControllerPrelude hiding (get, request)
import IHP.FrameworkConfig
import Generated.Types
import Web.Types
import Web.Routes ()
import Web.Controller.Posts ()
import Web.Job.UpdatePostViews ()
import Text.Blaze.Html (Html)

instance FrontController WebApplication where
    controllers = [ parseRoute @PostsController ]

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
    <html>
        <body>{inner}</body>
    </html>
|]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]
