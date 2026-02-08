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
import IHP.Hasql.FromRow (FromRowHasql)
import Network.Wai (Request)

{-# INLINE initAuthentication #-}
initAuthentication :: forall user normalizedModel.
        ( ?context :: ControllerContext
        , ?request :: Request
        , ?modelContext :: ModelContext
        , normalizedModel ~ NormalizeModel user
        , Typeable normalizedModel
        , Table normalizedModel
        , FromRow normalizedModel
        , FromRowHasql normalizedModel
        , PrimaryKey (GetTableName normalizedModel) ~ UUID
        , GetTableName normalizedModel ~ GetTableName user
        , FilterPrimaryKey (GetTableName normalizedModel)
        , KnownSymbol (GetModelName user)
    ) => IO ()
initAuthentication = do
    user <- getSession @(Id user) (sessionKey @user)
            >>= fetchOneOrNothing
    putContext user
