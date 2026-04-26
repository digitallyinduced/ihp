{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
    instance per monomorphic classic controller (whose @toControllerRoute@
    wraps @\<ctrlLower>Trie runAction'@ in a 'ControllerRouteTrie') and,
    for lowercase-header blocks, a @webRoutes :: [ControllerRoute app]@
    binding ready for @FrontController.controllers@. Lowercase bindings
    can also mount typed GADT actions and polymorphic framework controllers.
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
import Data.Dynamic (toDyn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Dec, Name)
import qualified Language.Haskell.TH.Quote as TH

import IHP.Controller.TypedAction (ActionDef, ParameterLocation (..))
import IHP.ControllerSupport (ControllerAction)
import IHP.Router.Capture (UrlCapture (..))
import IHP.Router.DSL.Runtime (buildRouteTrie)
import qualified IHP.ModelSupport as ModelSupport
import IHP.Router.TypedRoute
    ( DummyRouteValue (..)
    , HasActionMethods (..)
    , mkRouteParameterDoc
    , runTypedRouteAction
    , typedDocumentedRenderExpectationForAction
    , typedRouteDocumentForAction
    )
import IHP.Router.DSL.TH
    ( ParsedBlock (..)
    , HeaderForm (..)
    , ControllerInfo (..)
    , ConstructorInfo (..)
    , ValidatedRoute (..)
    , ValidatedSeg (..)
    , QueryFieldKind (..)
    , controllerAppliedType
    , expandHeadForGet
    , handlerExpr
    , patternExp
    , parseAndReify
    , genericEmit
    , routePathTemplate
    , trieValueName
    )
import Network.HTTP.Types.Method (StdMethod)

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
--   * one @instance CanRoute Ctrl@ per monomorphic classic controller,
--     whose @toControllerRoute@ wraps @\<ctrlLower>Trie runAction'@ in a
--     'ControllerRouteTrie';
--   * for a lowercase-header block: a top-level
--     @webRoutes :: [ControllerRoute app]@ binding that includes
--     @webSocketRoute \@T \"\/path\"@ entries for each @WS@ route in
--     the block alongside the regular @parseRoute \@Ctrl@ entries.
ihpEmit :: ParsedBlock -> Q [Dec]
ihpEmit ParsedBlock { pbHeader, pbGroups, pbWsRoutes } = do
    case pbHeader of
        HeaderLowercase _ -> pure ()
        _
            | any (ciIsGadt . fst) pbGroups ->
                fail "routes: typed GADT action routes must use a lowercase binding header, e.g. [routes|webRoutes ...|], because parseRoute cannot mount an indexed action family"
            | otherwise -> pure ()
    canRouteDecs <- traverse (\(ctrl, _) -> emitCanRoute ctrl) (filter shouldEmitCanRoute pbGroups)
    actionMethodDecs <- traverse (\(ctrl, routes) -> emitHasActionMethods ctrl routes) (filter (ciIsGadt . fst) pbGroups)
    bindingDecs <- case pbHeader of
        HeaderLowercase name -> emitNamedBinding name pbGroups pbWsRoutes
        _                    -> pure []
    pure (canRouteDecs <> actionMethodDecs <> bindingDecs)
  where
    shouldEmitCanRoute (ctrl, _) =
        not (ciIsGadt ctrl) && null (ciTypeVars ctrl)

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

hasActionMethodsClass, actionMethodsFn :: Name
hasActionMethodsClass = ''HasActionMethods
actionMethodsFn = 'actionMethods

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
        (TH.AppT (TH.ConT canRouteClass) (controllerAppliedType ctrl))
        [parseRouteDecl, toControllerRouteDecl])

emitHasActionMethods :: ControllerInfo -> [ValidatedRoute] -> Q Dec
emitHasActionMethods ctrl routesForController = do
    clauses <- traverse emitActionMethodsClause routesForController
    let fallback =
            TH.Clause
                [TH.WildP]
                (TH.NormalB (TH.ConE 'Nothing))
                []
    pure
        ( TH.InstanceD
            Nothing
            []
            (TH.AppT (TH.ConT hasActionMethodsClass) (controllerAppliedType ctrl))
            [TH.FunD actionMethodsFn (clauses <> [fallback])]
        )

emitActionMethodsClause :: ValidatedRoute -> Q TH.Clause
emitActionMethodsClause route =
    pure
        ( TH.Clause
            [constructorWildPattern (vrCon route)]
            (TH.NormalB (TH.AppE (TH.ConE 'Just) (methodsListExp (expandHeadForGet (vrMethods route)))))
            []
        )

constructorWildPattern :: ConstructorInfo -> TH.Pat
constructorWildPattern ConstructorInfo{coName, coFieldsOrder} =
    case coFieldsOrder of
        [] -> TH.ConP coName [] []
        fields -> TH.RecP coName [(TH.mkName (Text.unpack fieldName), TH.WildP) | (fieldName, _) <- fields]

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
emitNamedBinding :: Text -> [(ControllerInfo, [ValidatedRoute])] -> [(Name, ByteString)] -> Q [Dec]
emitNamedBinding bindingTxt groups wsBindings = do
    let valName = TH.mkName (Text.unpack bindingTxt)
        appTyVarName = TH.mkName "app"
        appTy = TH.VarT appTyVarName
    gadtEntries <- traverse (uncurry (emitGadtControllerRoute appTyVarName)) gadtGroups
    gadtCtx <- concat <$> traverse (uncurry gadtControllerConstraints) gadtGroups
    let
        webSocketRouteName = TH.mkName "webSocketRoute"

        classicEntries =
            [ classicControllerRouteExp appTy c
            | c <- classicCtrls
            ]
        wsEntries =
            [ TH.AppE
                (TH.AppTypeE (TH.VarE webSocketRouteName) (TH.ConT wsTy))
                (TH.SigE
                    (TH.LitE (TH.StringL (ByteString.Char8.unpack path)))
                    (TH.ConT (TH.mkName "ByteString")))
            | (wsTy, path) <- wsBindings
            ]
        entries =
            classicEntries <> gadtEntries <> wsEntries
        bindingExp = TH.ListE entries

        implicitReqTy  = TH.ImplicitParamT "request"  (TH.ConT (TH.mkName "Request"))
        implicitResTy  = TH.ImplicitParamT "respond"  (TH.ConT (TH.mkName "Respond"))
        implicitAppTy  = TH.ImplicitParamT "application" appTy
        initContextTy  = TH.AppT (TH.ConT (TH.mkName "InitControllerContext")) appTy
        typeableAppTy  = TH.AppT (TH.ConT ''Typeable) appTy
        perCtrl ctrl =
            let ctrlTy = controllerAppliedTypeForBinding appTy ctrl
             in [ TH.AppT (TH.ConT (TH.mkName "Controller")) ctrlTy
                , TH.AppT (TH.ConT ''Typeable) ctrlTy
                ]
        perWs (wsTy, _) =
            let ty = TH.ConT wsTy
             in [ TH.AppT (TH.ConT (TH.mkName "WSApp")) ty
                , TH.AppT (TH.ConT ''Typeable) ty
                ]
        ctx = implicitReqTy : implicitResTy : implicitAppTy
            : initContextTy : typeableAppTy
            : concatMap perCtrl classicCtrls
            <> gadtCtx
            <> concatMap perWs wsBindings
        resultTy = TH.AppT TH.ListT
            (TH.AppT (TH.ConT (TH.mkName "ControllerRoute")) appTy)
        bindingTy = TH.ForallT [TH.PlainTV appTyVarName TH.SpecifiedSpec] ctx resultTy

    pure
        [ TH.SigD valName bindingTy
        , TH.FunD valName [TH.Clause [] (TH.NormalB bindingExp) []]
        ]
    where
    classicCtrls = [ctrl | (ctrl, _) <- groups, not (ciIsGadt ctrl)]
    gadtGroups = [(ctrl, routesForController) | (ctrl, routesForController) <- groups, ciIsGadt ctrl]

controllerAppliedTypeForBinding :: TH.Type -> ControllerInfo -> TH.Type
controllerAppliedTypeForBinding appTy ControllerInfo{ciTypeName, ciTypeVars} =
    foldl TH.AppT (TH.ConT ciTypeName) (replicate (length ciTypeVars) appTy)

classicControllerRouteExp :: TH.Type -> ControllerInfo -> TH.Exp
classicControllerRouteExp appTy ctrl =
    TH.AppE
        (TH.ConE controllerRouteTrieCon)
        ( TH.AppE
            (TH.VarE (trieValueName (ciTypeName ctrl)))
            ( TH.AppTypeE
                (TH.AppTypeE (TH.VarE runActionPrimeFn) appTy)
                (controllerAppliedTypeForBinding appTy ctrl)
            )
        )

emitGadtControllerRoute :: Name -> ControllerInfo -> [ValidatedRoute] -> Q TH.Exp
emitGadtControllerRoute appTyVarName _ctrl routesForController = do
    entries <- traverse (emitGadtRouteEntry appTyVarName) routesForController
    documentExps <- traverse typedRouteDocumentExp routesForController
    let trieExp = TH.AppE (TH.VarE 'buildRouteTrie) (TH.ListE entries)
        docsExp =
            TH.AppE
                (TH.AppE (TH.VarE 'map) (TH.VarE 'toDyn))
                (TH.AppE (TH.VarE 'catMaybes) (TH.ListE documentExps))
        inspectionExp =
            TH.AppE
                (TH.ConE (TH.mkName "RouteLeaf"))
                ( TH.AppE
                    (TH.ConE (TH.mkName "DocumentedRoute"))
                    (TH.AppE (TH.ConE (TH.mkName "TypedRouteControllerInfo")) docsExp)
                )
    pure
        ( TH.AppE
            (TH.AppE (TH.ConE (TH.mkName "ControllerRouteTrie'")) trieExp)
            inspectionExp
        )

emitGadtRouteEntry :: Name -> ValidatedRoute -> Q TH.Exp
emitGadtRouteEntry appTyVarName route = do
    handler <- typedHandlerExpr appTyVarName route
    pure
        ( TH.TupE
            [ Just (methodsListExp (expandHeadForGet (vrMethods route)))
            , Just (patternExp (vrPath route))
            , Just handler
            ]
        )

typedHandlerExpr :: Name -> ValidatedRoute -> Q TH.Exp
typedHandlerExpr appTyVarName route = do
    runnerName <- TH.newName "_typedRouteRunner"
    typedActionName <- TH.newName "_typedAction"
    handler <- handlerExpr runnerName route
    let typedAction = TH.VarE typedActionName
        runner =
            TH.LamE
                [TH.VarP typedActionName]
                ( TH.AppE
                    ( TH.AppE
                        (TH.AppTypeE (TH.VarE 'runTypedRouteAction) (TH.VarT appTyVarName))
                        (TH.AppE (TH.VarE 'typedDocumentedRenderExpectationForAction) typedAction)
                    )
                    typedAction
                )
    pure (TH.LetE [TH.ValD (TH.VarP runnerName) (TH.NormalB runner) []] handler)

typedRouteDocumentExp :: ValidatedRoute -> Q TH.Exp
typedRouteDocumentExp route = do
    let constructor = vrCon route
        routeName = Text.pack (TH.nameBase (coName constructor))
        pathTemplate = routePathTemplate (vrPath route)
        params = routeParameterExps route
        dummyAction = dummyActionExp constructor
    pure
        ( foldl
            TH.AppE
            (TH.VarE 'typedRouteDocumentForAction)
            [ TH.LitE (TH.StringL (Text.unpack routeName))
            , TH.LitE (TH.StringL (Text.unpack pathTemplate))
            , methodsListExp (vrMethods route)
            , TH.ListE params
            , dummyAction
            ]
        )

routeParameterExps :: ValidatedRoute -> [TH.Exp]
routeParameterExps route =
    pathParameterExps <> queryParameterExps
  where
    pathParameterExps =
        [ routeParameterDocExp name ty 'PathParameter True
        | segment <- vrPath route
        , (name, ty) <- case segment of
            VSCapture captureName captureType -> [(captureName, captureType)]
            VSSplat captureName captureType -> [(captureName, captureType)]
            VSLiteral _ -> []
        ]

    queryParameterExps =
        [ routeParameterDocExp urlName (queryParameterSchemaType route fieldName kind innerType) 'QueryParameter (kind == QFRequired)
        | (urlName, fieldName, kind, innerType) <- vrQueryFields route
        ]

queryParameterSchemaType :: ValidatedRoute -> Text -> QueryFieldKind -> TH.Type -> TH.Type
queryParameterSchemaType route fieldName kind innerType =
    case kind of
        QFList -> fromMaybe innerType (lookup fieldName (coFieldsOrder (vrCon route)))
        QFRequired -> innerType
        QFOptional -> innerType

routeParameterDocExp :: Text -> TH.Type -> Name -> Bool -> TH.Exp
routeParameterDocExp name valueType location required =
    TH.AppE
        ( TH.AppE
            ( TH.AppTypeE
                (TH.AppTypeE (TH.VarE 'mkRouteParameterDoc) (TH.LitT (TH.StrTyLit (Text.unpack name))))
                valueType
            )
            (TH.ConE location)
        )
        (if required then TH.ConE 'True else TH.ConE 'False)

dummyActionExp :: ConstructorInfo -> TH.Exp
dummyActionExp ConstructorInfo{coName, coFieldsOrder} =
    case coFieldsOrder of
        [] -> TH.ConE coName
        fields ->
            TH.RecConE
                coName
                [ (TH.mkName (Text.unpack fieldName), TH.AppTypeE (TH.VarE 'dummyRouteValue) fieldType)
                | (fieldName, fieldType) <- fields
                ]

methodsListExp :: [StdMethod] -> TH.Exp
methodsListExp methods =
    TH.ListE [TH.ConE (TH.mkName (show method)) | method <- methods]

gadtControllerConstraints :: ControllerInfo -> [ValidatedRoute] -> Q [TH.Type]
gadtControllerConstraints ctrl routesForController =
    concat <$> traverse (gadtRouteConstraints ctrl) routesForController

gadtRouteConstraints :: ControllerInfo -> ValidatedRoute -> Q [TH.Type]
gadtRouteConstraints ctrl route = do
    (bodyTy, responseTy) <- actionBodyResponse ctrl (coResultType (vrCon route))
    let actionTy = coResultType (vrCon route)
        actionDefTy = TH.AppT (TH.AppT (TH.AppT (TH.ConT ''ActionDef) actionTy) bodyTy) responseTy
        controllerActionTy = TH.AppT (TH.ConT ''ControllerAction) actionTy
        controllerConstraints =
            [ TH.AppT (TH.ConT (TH.mkName "Controller")) actionTy
            , TH.AppT (TH.ConT ''Typeable) actionTy
            , TH.AppT (TH.AppT TH.EqualityT controllerActionTy) actionDefTy
            ]
        pathConstraints =
            [ routeValueConstraints captureType
            | segment <- vrPath route
            , captureType <- case segment of
                VSCapture _ ty -> [ty]
                VSSplat _ ty -> [ty]
                VSLiteral _ -> []
            ]
        queryConstraints =
            [ queryValueConstraints innerType
                <> schemaValueConstraints (queryParameterSchemaType route fieldName kind innerType)
                <> dummyValueConstraint fieldType
            | (_urlName, fieldName, kind, innerType) <- vrQueryFields route
            , fieldType <- maybeToList (lookup fieldName (coFieldsOrder (vrCon route)))
            ]
        fieldDummyConstraints =
            [ dummyValueConstraint fieldType
            | (_fieldName, fieldType) <- coFieldsOrder (vrCon route)
            ]
    pure (controllerConstraints <> concat pathConstraints <> concat queryConstraints <> concat fieldDummyConstraints)

routeValueConstraints :: TH.Type -> [TH.Type]
routeValueConstraints valueType =
    [ TH.AppT (TH.ConT ''UrlCapture) valueType
    , TH.AppT (TH.ConT ''ToSchema) valueType
    , TH.AppT (TH.ConT ''Typeable) valueType
    , TH.AppT (TH.ConT ''DummyRouteValue) valueType
    ]

queryValueConstraints :: TH.Type -> [TH.Type]
queryValueConstraints valueType =
    [ TH.AppT (TH.ConT ''UrlCapture) valueType
    , TH.AppT (TH.ConT ''ToSchema) valueType
    , TH.AppT (TH.ConT ''Typeable) valueType
    ]

schemaValueConstraints :: TH.Type -> [TH.Type]
schemaValueConstraints valueType =
    [ TH.AppT (TH.ConT ''ToSchema) valueType
    , TH.AppT (TH.ConT ''Typeable) valueType
    ]

dummyValueConstraint :: TH.Type -> [TH.Type]
dummyValueConstraint valueType =
    [TH.AppT (TH.ConT ''DummyRouteValue) valueType]

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just value) = [value]

actionBodyResponse :: ControllerInfo -> TH.Type -> Q (TH.Type, TH.Type)
actionBodyResponse ctrl actionTy =
    case unfoldTypeApps actionTy of
        (TH.ConT typeName, [bodyTy, responseTy])
            | typeName == ciTypeName ctrl -> pure (bodyTy, responseTy)
        _ ->
            fail
                ( "routes: typed GADT action family "
                    <> TH.nameBase (ciTypeName ctrl)
                    <> " must have exactly two type indices: request body and response view"
                )

unfoldTypeApps :: TH.Type -> (TH.Type, [TH.Type])
unfoldTypeApps = go []
  where
    go args = \case
        TH.AppT f x -> go (x : args) f
        ty -> (ty, args)
