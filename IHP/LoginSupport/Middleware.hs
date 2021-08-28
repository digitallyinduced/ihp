{-# LANGUAGE AllowAmbiguousTypes #-}

module IHP.LoginSupport.Middleware (initAuthentication) where

import IHP.Prelude
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
    user <- getSessionRecordId @user (sessionKey @user)
            >>= fetchOneOrNothing
    putContext user
