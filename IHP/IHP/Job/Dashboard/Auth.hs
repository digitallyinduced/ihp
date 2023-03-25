{-# LANGUAGE AllowAmbiguousTypes #-}

{-|
Module: IHP.Job.Dashboard.Auth
Description:  Authentication for Job dashboard
-}
module IHP.Job.Dashboard.Auth (
    AuthenticationMethod(..),
    NoAuth(..),
    BasicAuth(..),
    BasicAuthStatic(..),
) where

import IHP.Prelude
import GHC.TypeLits
import IHP.ControllerPrelude
import System.Environment (lookupEnv)

-- | Defines one method, 'authenticate', called before every action. Use to authenticate user.
--
-- Three implementations are provided:
-- - 'NoAuth' : No authentication
-- - 'BasicAuth' : HTTP Basic Auth using environment variables
-- - 'BasicAuthStatic' : HTTP Basic Auth using static values
--
-- Define your own implementation to use custom authentication for production.
class AuthenticationMethod a where
    authenticate :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()

-- | Don't use any authentication for jobs.
data NoAuth

-- | Authenticate using HTTP Basic Authentication by looking up username/password values
-- in environment variables given as type-level strings.
data BasicAuth (userEnv :: Symbol) (passEnv :: Symbol)

-- | Authenticate using HTTP Basic Authentication using username/password given as type level strings.
-- Meant for development only!
data BasicAuthStatic (user :: Symbol) (pass :: Symbol)

instance AuthenticationMethod NoAuth where
    authenticate = pure ()

instance (KnownSymbol userEnv, KnownSymbol passEnv) => AuthenticationMethod (BasicAuth userEnv passEnv) where
    authenticate = do
        creds <- (,) <$> lookupEnv (symbolVal $ Proxy @userEnv) <*> lookupEnv (symbolVal $ Proxy @passEnv)
        case creds of
            (Just user, Just pass) -> basicAuth (cs user) (cs pass) "jobs"
            _ -> error "Did not find HTTP Basic Auth credentials for Jobs Dashboard."

instance (KnownSymbol user, KnownSymbol pass) => AuthenticationMethod (BasicAuthStatic user pass) where
    authenticate = basicAuth (cs $ symbolVal $ Proxy @user) (cs $ symbolVal $ Proxy @pass) "jobs"

