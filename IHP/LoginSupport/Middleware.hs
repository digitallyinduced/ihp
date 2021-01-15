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
import IHP.Fetch
import IHP.ControllerSupport
import IHP.ModelSupport
import IHP.Controller.Context

{-# INLINE initAuthentication #-}
initAuthentication :: forall user.
        ( ?context :: ControllerContext
        , ?modelContext :: ModelContext
        , Typeable (NormalizeModel user)
        , KnownSymbol (GetTableName (NormalizeModel user))
        , KnownSymbol (GetModelName user)
        , GetTableName (NormalizeModel user) ~ GetTableName user
        , FromRow (NormalizeModel user)
        , PrimaryKey (GetTableName user) ~ UUID
        , FilterPrimaryKey (GetTableName user)
    ) => IO ()
initAuthentication = do
    user <- getSessionUUID (sessionKey @user)
            >>= pure . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    putContext user
