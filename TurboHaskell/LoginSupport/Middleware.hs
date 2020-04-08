{-# LANGUAGE AllowAmbiguousTypes #-}

module TurboHaskell.LoginSupport.Middleware (initAuthentication) where

import TurboHaskell.Prelude
import           Control.Exception
import Network.Wai (Application, Middleware, ResponseReceived)
import TurboHaskell.LoginSupport.Types
import qualified TurboHaskell.ControllerSupport as ControllerSupport
import TurboHaskell.FrameworkConfig (FrameworkConfig)
import qualified Data.TMap as TypeMap
import qualified Control.Newtype.Generics as Newtype
import TurboHaskell.LoginSupport.Helper.Controller
import TurboHaskell.Controller.Session
import TurboHaskell.QueryBuilder
import TurboHaskell.ControllerSupport

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
            >>= pure . fmap (Newtype.pack @(Id user))
            >>= fetchOneOrNothing
    pure (TypeMap.insert @(Maybe (NormalizeModel user)) user context)