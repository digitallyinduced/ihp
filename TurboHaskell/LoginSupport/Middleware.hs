{-# LANGUAGE AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Middleware (middleware, initAuthentication) where

import ClassyPrelude hiding (catch)
import           Control.Exception
import Network.Wai (Application, Middleware, ResponseReceived)
import TurboHaskell.ApplicationContext
import qualified TurboHaskell.LoginSupport.Controller
import TurboHaskell.LoginSupport.Types
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import TurboHaskell.FrameworkConfig (FrameworkConfig)

import qualified Data.TMap as TypeMap
import TurboHaskell.ModelSupport
import qualified Control.Newtype.Generics as Newtype
import TurboHaskell.LoginSupport.Helper.Controller
import TurboHaskell.Controller.Session
import TurboHaskell.QueryBuilder
import TurboHaskell.ControllerSupport
import GHC.Records
import GHC.TypeLits
import Database.PostgreSQL.Simple (FromRow)

middleware :: FrameworkConfig => ApplicationContext -> Middleware
middleware applicationContext application request respond = (application request respond) `catch` (\(e :: NotLoggedIn) -> handleNotLoggedIn e)
    where
        handleNotLoggedIn ::  TurboHaskell.LoginSupport.Types.NotLoggedIn -> IO ResponseReceived
        handleNotLoggedIn (TurboHaskell.LoginSupport.Types.NotLoggedIn newSessionUrl) = do
            requestContext <- ControllerSupport.createRequestContext applicationContext request respond
            let ?requestContext = requestContext
            let ?applicationContext = applicationContext
            let ?controllerContext = ControllerSupport.emptyControllerContext
            let ?modelContext = modelContext ?applicationContext
            ControllerSupport.runAction NotLoggedInAction { newSessionUrl }

{-# INLINE initAuthentication #-}
initAuthentication :: forall user.
        ( ?requestContext :: RequestContext
        , ?modelContext :: ModelContext
        , HasField "id" (NormalizeModel user) (Id user)
        , Typeable (NormalizeModel user)
        , KnownSymbol (GetTableName (NormalizeModel user))
        , KnownSymbol (GetModelName user)
        , FromRow (NormalizeModel user)
    ) => TypeMap.TMap -> IO TypeMap.TMap
initAuthentication context = do
    user <- getSessionUUID (sessionKey @user)
            >>= return . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    return (TypeMap.insert @(Maybe (NormalizeModel user)) user context)