module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)
import qualified IHP.DataSync.Role as Role

import IHP.DataSync.Types
import IHP.DataSync.Controller
import IHP.DataSync.REST.Types
import IHP.DataSync.REST.Controller

import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

-- Controller Imports
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        , webSocketApp @DataSyncController
        , parseRoute @ApiController
        , parseRoute @SessionsController
        -- Generator Marker
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
        Role.ensureAuthenticatedRoleExists