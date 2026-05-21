{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: IHP.Router.IHP
Description: IHP-flavoured wrapper around the @ihp-router@ DSL splice

The @ihp-router@ package ships an IHP-free @[routes|…|]@ quasi-quoter
that emits 'HasPath' instances and a parameterised
@\<ctrlLower>Trie :: (Ctrl -> Application) -> RouteTrie@ binding per
controller. Plain WAI users wire that binding into
'IHP.Router.Middleware.routeTrieMiddleware' with their own dispatch
function.

This module is the IHP-specific shim that composes on top:

  * 'routes' \/ 'routesDec' — IHP-flavoured quoter. Emits everything
    'IHP.Router.DSL.TH.genericEmit' produces, plus a 'CanRoute'
    instance per controller (whose @toControllerRoute@ wraps
    @\<ctrlLower>Trie runAction'@ in a 'ControllerRouteTrie') and, for
    lowercase-header blocks, a @webRoutes :: [ControllerRoute app]@
    binding ready for @FrontController.controllers@.
  * 'instance UrlCapture (Id' table)' — IHP's primary-key-driven
    capture. Lives here (not in @ihp-router@) because it needs
    'IHP.ModelSupport.PrimaryKey'.

User code accesses the IHP-flavoured quoter as
@import IHP.Router.DSL (routes)@, which re-exports from this module.
The user-visible import surface is unchanged from before the
extraction.
-}
module IHP.Router.IHP
    ( routes
    , routesDec
    , ihpRoutesDec
    , ihpEmit
    ) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Dec, Name)
import qualified Language.Haskell.TH.Quote as TH

import IHP.Router.Capture (UrlCapture (..))
import qualified IHP.ModelSupport as ModelSupport
import IHP.Router.DSL.TH
    ( ParsedBlock (..)
    , HeaderForm (..)
    , ControllerInfo (..)
    , parseAndReify
    , genericEmit
    , trieValueName
    )

-- | Captures for IHP 'Id' values route through the table's primary-key type.
-- This works for any table whose 'ModelSupport.PrimaryKey' has a 'UrlCapture'
-- instance — 'UUID', 'Int', 'Integer', 'Text', etc.
--
-- Lives in the IHP shim so that @ihp-router@ can ship without dragging
-- in 'IHP.ModelSupport'. Plain WAI users get the base instances on
-- 'Text' \/ 'Int' \/ 'UUID' etc. from "IHP.Router.Capture"; IHP apps
-- get this orphan in scope automatically through @import IHP.RouterPrelude@.
instance
    ( Typeable table
    , Typeable (ModelSupport.PrimaryKey table)
    , UrlCapture (ModelSupport.PrimaryKey table)
    ) => UrlCapture (ModelSupport.Id' table) where
    parseCapture bs = ModelSupport.Id <$> parseCapture @(ModelSupport.PrimaryKey table) bs
    {-# INLINE parseCapture #-}
    renderCapture (ModelSupport.Id pk) = renderCapture pk
    {-# INLINE renderCapture #-}

-- | The IHP-flavoured @[routes|…|]@ quasi-quoter. Behaves identically
-- to the pre-extraction quoter — re-exports the same 'routesDec' that
-- composes 'genericEmit' with the IHP-specific 'ihpEmit'.
--
-- Use as a top-level declaration in @Web/Routes.hs@:
--
-- > [routes|webRoutes
-- > GET    /Posts                 PostsAction
-- > GET    /ShowPost?postId       ShowPostAction
-- > |]
-- >
-- > instance FrontController WebApplication where
-- >     controllers = webRoutes
routes :: TH.QuasiQuoter
routes = TH.QuasiQuoter
    { TH.quoteExp  = \_ -> fail
        ( "routes: the [routes|…|] quoter must be used as a top-level "
        <> "declaration, not an expression. "
        <> "Put the binding name in the header line:\n\n"
        <> "    [routes|webRoutes\n"
        <> "    GET /posts PostsIndexAction\n"
        <> "    |]\n\n"
        <> "TH can't emit class instances from expression splices, so "
        <> "expression-form usage isn't supported." )
    , TH.quotePat  = \_ -> fail "routes: use as a top-level declaration"
    , TH.quoteType = \_ -> fail "routes: use as a top-level declaration"
    , TH.quoteDec  = routesDec
    }

-- | Underlying TH function for the IHP-flavoured 'routes' quoter.
-- Composes 'genericEmit' (HasPath + per-controller @\<ctrlLower>Trie@
-- bindings) with 'ihpEmit' (IHP @CanRoute@ instance + lowercase-header
-- binding).
routesDec :: String -> Q [Dec]
routesDec source = do
    block <- parseAndReify source
    generic <- genericEmit block
    ihp <- ihpEmit block
    pure (generic <> ihp)

-- | Alias for 'routesDec'. Exposed so callers that already use the
-- generic 'IHP.Router.DSL.TH.genericRoutesDec' have a matching named
-- entry point on the IHP-flavoured side.
ihpRoutesDec :: String -> Q [Dec]
ihpRoutesDec = routesDec

-- | Emit the IHP-flavoured declarations on top of whatever 'genericEmit'
-- produces:
--
--   * one @instance CanRoute Ctrl@ per controller, whose
--     @toControllerRoute@ wraps @\<ctrlLower>Trie runAction'@ in a
--     'ControllerRouteTrie';
--   * for a lowercase-header block: a top-level
--     @webRoutes :: [ControllerRoute app]@ binding that includes
--     @webSocketRoute \@T \"\/path\"@ entries for each @WS@ route in
--     the block alongside the regular @parseRoute \@Ctrl@ entries.
ihpEmit :: ParsedBlock -> Q [Dec]
ihpEmit ParsedBlock { pbHeader, pbGroups, pbWsRoutes } = do
    canRouteDecs <- traverse (\(ctrl, _) -> emitCanRoute ctrl) pbGroups
    bindingDecs <- case pbHeader of
        HeaderLowercase name ->
            emitNamedBinding name (map fst pbGroups) pbWsRoutes
        _ -> pure []
    pure (canRouteDecs <> bindingDecs)

---------------------------------------------------------------------------
-- IHP-specific TH names (resolved at splice use-site)
---------------------------------------------------------------------------

canRouteClass, parseRoutePrimeFn, toControllerRouteFn :: Name
canRouteClass       = TH.mkName "CanRoute"
parseRoutePrimeFn   = TH.mkName "parseRoute'"
toControllerRouteFn = TH.mkName "toControllerRoute"

controllerRouteTrieCon, runActionPrimeFn :: Name
controllerRouteTrieCon = TH.mkName "ControllerRouteTrie"
runActionPrimeFn       = TH.mkName "runAction'"

---------------------------------------------------------------------------
-- Code generation — CanRoute
---------------------------------------------------------------------------

-- | Emit the IHP-flavoured @instance CanRoute Controller@. The body
-- of @toControllerRoute@ references the generic top-level binding
-- emitted by 'IHP.Router.DSL.TH.emitTrieValue' —
-- @\<ctrlLower>Trie runAction'@ — so the generic and IHP halves share
-- one trie expression per controller.
emitCanRoute :: ControllerInfo -> Q Dec
emitCanRoute ctrl = do
    let trieValE = TH.AppE
            (TH.VarE (trieValueName (ciTypeName ctrl)))
            (TH.VarE runActionPrimeFn)
    -- parseRoute' is unused (the trie owns dispatch) but CanRoute still
    -- requires it. Emit `fail "..."` — MonadFail is in base so no extra
    -- import is needed at the call site.
    let parseRouteDecl = TH.FunD parseRoutePrimeFn
            [TH.Clause []
                (TH.NormalB
                    (TH.AppE (TH.VarE 'fail)
                        (TH.LitE (TH.StringL
                            "routes: parseRoute' is unused; dispatch goes through the trie"))))
                []]
        toControllerRouteDecl = TH.FunD toControllerRouteFn
            [TH.Clause []
                (TH.NormalB
                    (TH.AppE (TH.ConE controllerRouteTrieCon) trieValE))
                []]
    pure (TH.InstanceD Nothing []
        (TH.AppT (TH.ConT canRouteClass) (TH.ConT (ciTypeName ctrl)))
        [parseRouteDecl, toControllerRouteDecl])

---------------------------------------------------------------------------
-- Code generation — named binding for lowercase-header form
---------------------------------------------------------------------------

-- | Emit a top-level binding named by the user. Given the header
-- @webRoutes@, HTTP controllers @[PostsController, UsersController]@,
-- and a WS route @WS \/chat ChatApp@:
--
-- > webRoutes =
-- >     [ parseRoute @PostsController
-- >     , parseRoute @UsersController
-- >     , webSocketRoute @ChatApp "\/chat"
-- >     ]
--
-- The binding is polymorphic in the application type; when splatted into
-- 'FrontController.controllers' for a concrete app, GHC infers the right
-- @app@.
--
-- @parseRoute@ carries implicit-parameter constraints ('?request',
-- '?respond', '?application', plus 'Controller', 'CanRoute',
-- 'InitControllerContext', and 'Typeable'); @webSocketRoute@ carries
-- the same implicits but swaps 'Controller' \/ 'CanRoute' for 'WSApp'.
-- GHC cannot abstract those at the use site, so we emit an explicit
-- signature enumerating all of them.
emitNamedBinding :: Text -> [ControllerInfo] -> [(Name, ByteString)] -> Q [Dec]
emitNamedBinding bindingTxt ctrls wsBindings = do
    let valName = TH.mkName (Text.unpack bindingTxt)
        appTyVarName = TH.mkName "app"
        appTy = TH.VarT appTyVarName
        parseRouteName = TH.mkName "parseRoute"
        webSocketRouteName = TH.mkName "webSocketRoute"

        httpEntries =
            [ TH.AppTypeE (TH.VarE parseRouteName) (TH.ConT (ciTypeName c))
            | c <- ctrls
            ]
        wsEntries =
            [ TH.AppE
                (TH.AppTypeE (TH.VarE webSocketRouteName) (TH.ConT wsTy))
                (TH.SigE
                    (TH.LitE (TH.StringL (ByteString.Char8.unpack path)))
                    (TH.ConT (TH.mkName "ByteString")))
            | (wsTy, path) <- wsBindings
            ]
        bindingExp = TH.ListE (httpEntries <> wsEntries)

        implicitReqTy  = TH.ImplicitParamT "request"  (TH.ConT (TH.mkName "Request"))
        implicitResTy  = TH.ImplicitParamT "respond"  (TH.ConT (TH.mkName "Respond"))
        implicitAppTy  = TH.ImplicitParamT "application" appTy
        initContextTy  = TH.AppT (TH.ConT (TH.mkName "InitControllerContext")) appTy
        typeableAppTy  = TH.AppT (TH.ConT ''Typeable) appTy
        perCtrl ctrl =
            let ctrlTy = TH.ConT (ciTypeName ctrl)
             in [ TH.AppT (TH.ConT (TH.mkName "Controller")) ctrlTy
                , TH.AppT (TH.ConT (TH.mkName "CanRoute")) ctrlTy
                , TH.AppT (TH.ConT ''Typeable) ctrlTy
                ]
        perWs (wsTy, _) =
            let ty = TH.ConT wsTy
             in [ TH.AppT (TH.ConT (TH.mkName "WSApp")) ty
                , TH.AppT (TH.ConT ''Typeable) ty
                ]
        ctx = implicitReqTy : implicitResTy : implicitAppTy
            : initContextTy : typeableAppTy
            : concatMap perCtrl ctrls
            <> concatMap perWs wsBindings
        resultTy = TH.AppT TH.ListT
            (TH.AppT (TH.ConT (TH.mkName "ControllerRoute")) appTy)
        bindingTy = TH.ForallT [TH.PlainTV appTyVarName TH.SpecifiedSpec] ctx resultTy

    pure
        [ TH.SigD valName bindingTy
        , TH.FunD valName [TH.Clause [] (TH.NormalB bindingExp) []]
        ]
