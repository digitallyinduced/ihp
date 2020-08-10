{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Middleware (initAuthentication) where

import IHP.Prelude
import           Control.Exception
import Network.Wai (Application, Middleware, ResponseReceived)
import IHP.LoginSupport.Types
import qualified IHP.ControllerSupport as ControllerSupport
import IHP.FrameworkConfig (FrameworkConfig)
import qualified Data.TMap as TypeMap
import qualified Control.Newtype.Generics as Newtype
import IHP.LoginSupport.Helper.Controller
import IHP.Controller.Session
import IHP.QueryBuilder
import IHP.ControllerSupport
import IHP.ModelSupport

{-# INLINE initAuthentication #-}
initAuthentication :: forall user.
        ( ?requestContext :: RequestContext
        , ?modelContext :: ModelContext
        , HasField "id" (NormalizeModel user) (Id user)
        , Typeable (NormalizeModel user)
        , KnownSymbol (GetTableName (NormalizeModel user))
        , KnownSymbol (GetModelName user)
        , FromRow (NormalizeModel user)
        , PrimaryKey (GetTableName user) ~ UUID
    ) => TypeMap.TMap -> IO TypeMap.TMap
initAuthentication context = do
    user <- getSessionUUID (sessionKey @user)
            >>= pure . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    pure (TypeMap.insert @(Maybe (NormalizeModel user)) user context)