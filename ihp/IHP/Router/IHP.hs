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
import Control.Monad (foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Dynamic (toDyn)
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Dec, Name)
import qualified Language.Haskell.TH.Quote as TH
import Text.Read (readMaybe)

import IHP.Controller.TypedAction (BuildTypedRequestBodyDoc, DecodeRequest, ParameterLocation (..), RenderTypedResponse, TypedController, TypedRouteResponseDocument (..), mkTypedRouteDocument)
import IHP.Router.Capture (UrlCapture (..))
import IHP.Router.DSL.AST (RouteAnnotation (..))
import IHP.Router.DSL.Runtime (buildRouteTrie)
import qualified IHP.ModelSupport as ModelSupport
import IHP.Router.TypedRoute
    ( HasActionMethods (..)
    , mkTypedRouteResponseHandler
    , mkRouteParameterDoc
    , runTypedRouteAction
    , typedDocumentedRenderExpectationForResponse
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
import Network.HTTP.Types.Status (mkStatus)
import qualified IHP.ViewSupport as ViewSupport

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
    rejectClassicRouteAnnotations pbGroups
    canRouteDecs <- traverse (\(ctrl, _) -> emitCanRoute ctrl) (filter shouldEmitCanRoute pbGroups)
    actionMethodDecs <- traverse (\(ctrl, routes) -> emitHasActionMethods ctrl routes) (filter (ciIsGadt . fst) pbGroups)
    bindingDecs <- case pbHeader of
        HeaderLowercase name -> emitNamedBinding name pbGroups pbWsRoutes
        _                    -> pure []
    pure (canRouteDecs <> actionMethodDecs <> bindingDecs)
  where
    shouldEmitCanRoute (ctrl, _) =
        not (ciIsGadt ctrl) && null (ciTypeVars ctrl)

rejectClassicRouteAnnotations :: [(ControllerInfo, [ValidatedRoute])] -> Q ()
rejectClassicRouteAnnotations groups =
    mapM_
        ( \(ctrl, routesForController) ->
            if ciIsGadt ctrl
                then pure ()
                else mapM_ rejectRoute routesForController
        )
        groups
  where
    rejectRoute route =
        case vrAnnotations route of
            [] -> pure ()
            annotation : _ ->
                fail
                    ( "routes (line "
                        <> show (annotationLine annotation)
                        <> "): route metadata is only supported for typed GADT routes"
                    )

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

data TypedRouteMetadata = TypedRouteMetadata
    { routeSummary :: Maybe Text
    , routeDescription :: Maybe Text
    , routeTags :: [Text]
    , routeOperationId :: Maybe Text
    , routeSuccessStatusCode :: Int
    , routeSuccessResponseDescription :: Text
    , routeSuccessSpecified :: Bool
    , routeResponses :: [RouteResponseMetadata]
    , routePrivate :: Bool
    }

data RouteResponseMetadata = RouteResponseMetadata
    { responseConstructorName :: Text
    , responseStatusCode :: Int
    , responseDescription :: Text
    , responseAnnotationLine :: Int
    }

data RouteResponseSpec = RouteResponseSpec
    { responseSpecConstructorName :: Name
    , responseSpecConstructorText :: Text
    , responseSpecViewType :: TH.Type
    , responseSpecStatusCode :: Int
    , responseSpecDescription :: Text
    }

defaultTypedRouteMetadata :: TypedRouteMetadata
defaultTypedRouteMetadata =
    TypedRouteMetadata
        { routeSummary = Nothing
        , routeDescription = Nothing
        , routeTags = []
        , routeOperationId = Nothing
        , routeSuccessStatusCode = 200
        , routeSuccessResponseDescription = "Successful response"
        , routeSuccessSpecified = False
        , routeResponses = []
        , routePrivate = False
        }

typedRouteMetadata :: ValidatedRoute -> Q TypedRouteMetadata
typedRouteMetadata route = do
    rejectDuplicateAnnotations route
    metadata <- foldM applyAnnotation defaultTypedRouteMetadata (vrAnnotations route)
    if routeSuccessSpecified metadata && not (null (routeResponses metadata))
        then fail ("routes (line " <> show (vrLine route) <> "): use either 'success' or 'response Constructor' metadata, not both")
        else pure metadata

rejectDuplicateAnnotations :: ValidatedRoute -> Q ()
rejectDuplicateAnnotations route =
    case findDuplicate (map annotationName (vrAnnotations route)) of
        Nothing -> pure ()
        Just duplicate ->
            fail
                ( "routes (line "
                    <> show (vrLine route)
                    <> "): metadata key '"
                    <> Text.unpack duplicate
                    <> "' is declared more than once"
                )

findDuplicate :: Eq a => [a] -> Maybe a
findDuplicate = go []
  where
    go _ [] = Nothing
    go seen (value : rest)
        | value `elem` seen = Just value
        | otherwise = go (value : seen) rest

applyAnnotation :: TypedRouteMetadata -> RouteAnnotation -> Q TypedRouteMetadata
applyAnnotation metadata annotation@RouteAnnotation{annotationName, annotationValue, annotationLine} =
    case Text.words annotationName of
        ["summary"] -> do
            value <- requiredAnnotationValue annotation
            pure metadata{routeSummary = Just value}
        ["description"] -> do
            value <- requiredAnnotationValue annotation
            pure metadata{routeDescription = Just value}
        ["tags"] -> do
            value <- requiredAnnotationValue annotation
            parsedTags <- parseTagsAnnotation annotation value
            pure metadata{routeTags = parsedTags}
        ["operationId"] -> do
            value <- requiredAnnotationValue annotation
            pure metadata{routeOperationId = Just value}
        ["success"] -> do
            (statusCode, responseDescription) <- parseSuccessAnnotation annotation
            pure
                metadata
                    { routeSuccessStatusCode = statusCode
                    , routeSuccessResponseDescription = responseDescription
                    , routeSuccessSpecified = True
                    }
        ["response", constructorName] -> do
            (statusCode, description) <- parseStatusDescriptionAnnotation annotation "response metadata"
            let routeResponse =
                    RouteResponseMetadata
                        { responseConstructorName = constructorName
                        , responseStatusCode = statusCode
                        , responseDescription = description
                        , responseAnnotationLine = annotationLine
                        }
            pure metadata{routeResponses = routeResponses metadata <> [routeResponse]}
        ["response"] ->
            fail
                ( "routes (line "
                    <> show annotationLine
                    <> "): response metadata must name a response constructor, e.g. 'response Created: 201 Created'"
                )
        ["private"] ->
            case annotationValue of
                Nothing -> pure metadata{routePrivate = True}
                Just _ ->
                    fail
                        ( "routes (line "
                            <> show annotationLine
                            <> "): metadata key 'private' does not accept a value"
                        )
        _ ->
            fail
                ( "routes (line "
                    <> show annotationLine
                    <> "): unsupported typed route metadata key '"
                    <> Text.unpack annotationName
                    <> "'"
                )

requiredAnnotationValue :: RouteAnnotation -> Q Text
requiredAnnotationValue RouteAnnotation{annotationName, annotationValue, annotationLine} =
    case annotationValue of
        Just value | not (Text.null value) -> pure value
        _ ->
            fail
                ( "routes (line "
                    <> show annotationLine
                    <> "): metadata key '"
                    <> Text.unpack annotationName
                    <> "' requires a value after ':'"
                )

parseTagsAnnotation :: RouteAnnotation -> Text -> Q [Text]
parseTagsAnnotation RouteAnnotation{annotationLine} value =
    let parsed = map Text.strip (Text.splitOn "," value)
     in if null parsed || any Text.null parsed
            then fail ("routes (line " <> show annotationLine <> "): tags metadata must contain comma-separated non-empty tag names")
            else pure parsed

parseSuccessAnnotation :: RouteAnnotation -> Q (Int, Text)
parseSuccessAnnotation annotation =
    parseStatusDescriptionAnnotation annotation "success metadata"

parseStatusDescriptionAnnotation :: RouteAnnotation -> String -> Q (Int, Text)
parseStatusDescriptionAnnotation annotation@RouteAnnotation{annotationLine} label = do
    value <- requiredAnnotationValue annotation
    case Text.words value of
        [] ->
            fail ("routes (line " <> show annotationLine <> "): " <> label <> " must start with a status code")
        statusText : descriptionWords ->
            case readMaybe (Text.unpack statusText) of
                Just statusCode | statusCode >= 100 && statusCode <= 599 ->
                    let responseDescription = Text.unwords descriptionWords
                     in pure
                            ( statusCode
                            , if Text.null responseDescription then "Successful response" else responseDescription
                            )
                _ ->
                    fail
                        ( "routes (line "
                            <> show annotationLine
                            <> "): "
                            <> label
                            <> " must start with a numeric HTTP status code"
                        )

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
-- Classic @parseRoute@ entries carry implicit-parameter constraints
-- ('?request', '?respond', '?application'), plus 'Controller', 'CanRoute',
-- 'InitControllerContext', and 'Typeable'. Typed GADT entries add their
-- 'TypedController' and typed body/response constraints directly.
-- @webSocketRoute@ carries the same implicits but swaps controller constraints
-- for 'WSApp'. GHC cannot abstract those at the use site, so we emit an
-- explicit signature enumerating all of them.
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
emitGadtControllerRoute appTyVarName ctrl routesForController = do
    entries <- traverse (emitGadtRouteEntry appTyVarName ctrl) routesForController
    documentExps <- traverse (typedRouteDocumentExp ctrl) routesForController
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

emitGadtRouteEntry :: Name -> ControllerInfo -> ValidatedRoute -> Q TH.Exp
emitGadtRouteEntry appTyVarName ctrl route = do
    handler <- typedHandlerExpr appTyVarName ctrl route
    pure
        ( TH.TupE
            [ Just (methodsListExp (expandHeadForGet (vrMethods route)))
            , Just (patternExp (vrPath route))
            , Just handler
            ]
        )

typedHandlerExpr :: Name -> ControllerInfo -> ValidatedRoute -> Q TH.Exp
typedHandlerExpr appTyVarName ctrl route = do
    runnerName <- TH.newName "_typedRouteRunner"
    typedActionName <- TH.newName "_typedAction"
    (bodyTy, responseTy) <- actionBodyResponse ctrl (coResultType (vrCon route))
    metadata <- typedRouteMetadata route
    responseHandlers <- typedRouteResponseHandlerExps ctrl route responseTy metadata
    handler <- handlerExpr runnerName route
    let typedAction = TH.VarE typedActionName
        runner =
            TH.LamE
                [TH.VarP typedActionName]
                ( foldl
                    TH.AppE
                    ( TH.AppTypeE
                        ( TH.AppTypeE
                            ( TH.AppTypeE
                                (TH.AppTypeE (TH.VarE 'runTypedRouteAction) (TH.VarT appTyVarName))
                                (TH.ConT (ciTypeName ctrl))
                            )
                            bodyTy
                        )
                        responseTy
                    )
                    [ TH.ListE responseHandlers
                    , typedAction
                    ]
                )
    pure (TH.LetE [TH.ValD (TH.VarP runnerName) (TH.NormalB runner) []] handler)

typedRouteDocumentExp :: ControllerInfo -> ValidatedRoute -> Q TH.Exp
typedRouteDocumentExp ctrl route = do
    metadata <- typedRouteMetadata route
    let constructor = vrCon route
        routeName = Text.pack (TH.nameBase (coName constructor))
        pathTemplate = routePathTemplate (vrPath route)
        params = routeParameterExps route
    if routePrivate metadata
        then pure (TH.ConE 'Nothing)
        else do
            (bodyTy, responseTy) <- actionBodyResponse ctrl (coResultType constructor)
            responseDocuments <- typedRouteResponseDocumentExps ctrl route responseTy metadata
            pure
                ( TH.AppE
                    (TH.ConE 'Just)
                    ( foldl
                        TH.AppE
                        ( TH.AppTypeE
                            (TH.VarE 'mkTypedRouteDocument)
                            bodyTy
                        )
                        [ textExp routeName
                        , textExp pathTemplate
                        , methodsListExp (vrMethods route)
                        , TH.ListE params
                        , maybeTextExp (routeSummary metadata)
                        , maybeTextExp (routeDescription metadata)
                        , TH.ListE (map textExp (routeTags metadata))
                        , maybeTextExp (routeOperationId metadata)
                        , TH.ListE responseDocuments
                        ]
                    )
                )

typedRouteResponseHandlerExps :: ControllerInfo -> ValidatedRoute -> TH.Type -> TypedRouteMetadata -> Q [TH.Exp]
typedRouteResponseHandlerExps ctrl route responseTy metadata =
    case routeResponses metadata of
        [] -> pure [singleTypedRouteResponseHandlerExp responseTy metadata]
        _ -> do
            specs <- typedRouteResponseSpecs ctrl route responseTy metadata
            pure (map (typedRouteResponseHandlerExp responseTy metadata) specs)

typedRouteResponseDocumentExps :: ControllerInfo -> ValidatedRoute -> TH.Type -> TypedRouteMetadata -> Q [TH.Exp]
typedRouteResponseDocumentExps ctrl route responseTy metadata =
    case routeResponses metadata of
        [] -> pure [typedRouteResponseDocumentExp "success" responseTy (routeSuccessStatusCode metadata) (routeSuccessResponseDescription metadata)]
        _ -> do
            specs <- typedRouteResponseSpecs ctrl route responseTy metadata
            pure
                [ typedRouteResponseDocumentExp responseSpecConstructorText responseSpecViewType responseSpecStatusCode responseSpecDescription
                | RouteResponseSpec{responseSpecConstructorText, responseSpecViewType, responseSpecStatusCode, responseSpecDescription} <- specs
                ]

typedRouteResponseSpecs :: ControllerInfo -> ValidatedRoute -> TH.Type -> TypedRouteMetadata -> Q [RouteResponseSpec]
typedRouteResponseSpecs _ctrl route responseTy metadata = do
    rejectDuplicateResponseStatuses route metadata
    constructors <- responseTypeConstructors route responseTy
    substitution <- responseTypeSubstitution route responseTy
    specs <- traverse (routeResponseSpec route substitution constructors) (routeResponses metadata)
    let annotatedNames = map responseSpecConstructorName specs
        missingConstructors =
            [ constructorName
            | constructor <- constructors
            , constructorName <- responseConstructorNames constructor
            , constructorName `notElem` annotatedNames
            ]
    case missingConstructors of
        [] -> pure specs
        missing ->
            fail
                ( "routes (line "
                    <> show (vrLine route)
                    <> "): response metadata is missing for constructor(s): "
                    <> commaSeparatedNames missing
                )

rejectDuplicateResponseStatuses :: ValidatedRoute -> TypedRouteMetadata -> Q ()
rejectDuplicateResponseStatuses route metadata =
    case findDuplicate (map responseStatusCode (routeResponses metadata)) of
        Nothing -> pure ()
        Just duplicate ->
            fail
                ( "routes (line "
                    <> show (vrLine route)
                    <> "): response metadata declares HTTP status "
                    <> show duplicate
                    <> " more than once"
                )

routeResponseSpec :: ValidatedRoute -> [(Name, TH.Type)] -> [TH.Con] -> RouteResponseMetadata -> Q RouteResponseSpec
routeResponseSpec route substitution constructors RouteResponseMetadata{responseConstructorName, responseStatusCode, responseDescription, responseAnnotationLine} = do
    constructor <-
        case find (constructorHasTextName responseConstructorName) constructors of
            Just constructor -> pure constructor
            Nothing ->
                fail
                    ( "routes (line "
                        <> show responseAnnotationLine
                        <> "): response constructor '"
                        <> Text.unpack responseConstructorName
                        <> "' does not belong to the route response type"
                    )
    constructorName <-
        case filter ((== Text.unpack responseConstructorName) . TH.nameBase) (responseConstructorNames constructor) of
            [name] -> pure name
            _ ->
                fail
                    ( "routes (line "
                        <> show responseAnnotationLine
                        <> "): could not resolve response constructor '"
                        <> Text.unpack responseConstructorName
                        <> "'"
                    )
    viewType <-
        case responseConstructorFieldTypes constructor of
            [fieldType] -> pure (substituteType substitution fieldType)
            [] ->
                fail
                    ( "routes (line "
                        <> show responseAnnotationLine
                        <> "): response constructor '"
                        <> Text.unpack responseConstructorName
                        <> "' must wrap one view value"
                    )
            _ ->
                fail
                    ( "routes (line "
                        <> show responseAnnotationLine
                        <> "): response constructor '"
                        <> Text.unpack responseConstructorName
                        <> "' must wrap exactly one view value"
                    )
    pure
        RouteResponseSpec
            { responseSpecConstructorName = constructorName
            , responseSpecConstructorText = responseConstructorName
            , responseSpecViewType = viewType
            , responseSpecStatusCode = responseStatusCode
            , responseSpecDescription = responseDescription
            }

responseTypeConstructors :: ValidatedRoute -> TH.Type -> Q [TH.Con]
responseTypeConstructors route responseTy =
    case responseTypeName responseTy of
        Nothing ->
            fail
                ( "routes (line "
                    <> show (vrLine route)
                    <> "): response metadata requires a concrete response sum type"
                )
        Just typeName -> do
            TH.reify typeName >>= \case
                TH.TyConI (TH.DataD _ _ _ _ constructors _) -> pure constructors
                TH.TyConI (TH.NewtypeD _ _ _ _ constructor _) -> pure [constructor]
                _ ->
                    fail
                        ( "routes (line "
                            <> show (vrLine route)
                            <> "): response metadata requires a data or newtype response type"
                        )

responseTypeSubstitution :: ValidatedRoute -> TH.Type -> Q [(Name, TH.Type)]
responseTypeSubstitution route responseTy =
    case unfoldTypeApps responseTy of
        (TH.ConT typeName, args) -> do
            typeVariables <- responseTypeVariables route typeName
            case (args, typeVariables) of
                ([], _) -> pure []
                _ | length args == length typeVariables -> pure (zip typeVariables args)
                _ ->
                    fail
                        ( "routes (line "
                            <> show (vrLine route)
                            <> "): response metadata requires a fully applied response type"
                        )
        _ -> pure []

responseTypeVariables :: ValidatedRoute -> Name -> Q [Name]
responseTypeVariables route typeName =
    TH.reify typeName >>= \case
        TH.TyConI (TH.DataD _ _ typeVariables _ _ _) -> pure (map typeVariableName typeVariables)
        TH.TyConI (TH.NewtypeD _ _ typeVariables _ _ _) -> pure (map typeVariableName typeVariables)
        _ ->
            fail
                ( "routes (line "
                    <> show (vrLine route)
                    <> "): response metadata requires a data or newtype response type"
                )

typeVariableName :: TH.TyVarBndr flag -> Name
typeVariableName = \case
    TH.PlainTV name _ -> name
    TH.KindedTV name _ _ -> name

responseTypeName :: TH.Type -> Maybe Name
responseTypeName responseTy =
    case fst (unfoldTypeApps responseTy) of
        TH.ConT name -> Just name
        _ -> Nothing

substituteType :: [(Name, TH.Type)] -> TH.Type -> TH.Type
substituteType substitution = go
  where
    go ty@(TH.VarT name) = fromMaybe ty (lookup name substitution)
    go (TH.AppT left right) = TH.AppT (go left) (go right)
    go (TH.AppKindT left right) = TH.AppKindT (go left) right
    go (TH.SigT ty kind) = TH.SigT (go ty) kind
    go (TH.InfixT left name right) = TH.InfixT (go left) name (go right)
    go (TH.UInfixT left name right) = TH.UInfixT (go left) name (go right)
    go (TH.ParensT ty) = TH.ParensT (go ty)
    go (TH.ForallT binders context ty) =
        let boundNames = map typeVariableName binders
            scopedSubstitution = filter (\(name, _) -> name `notElem` boundNames) substitution
         in TH.ForallT binders (map (substituteType scopedSubstitution) context) (substituteType scopedSubstitution ty)
    go other = other

constructorHasTextName :: Text -> TH.Con -> Bool
constructorHasTextName expected constructor =
    any ((== Text.unpack expected) . TH.nameBase) (responseConstructorNames constructor)

responseConstructorNames :: TH.Con -> [Name]
responseConstructorNames = \case
    TH.NormalC name _ -> [name]
    TH.RecC name _ -> [name]
    TH.InfixC _ name _ -> [name]
    TH.ForallC _ _ constructor -> responseConstructorNames constructor
    TH.GadtC names _ _ -> names
    TH.RecGadtC names _ _ -> names

responseConstructorFieldTypes :: TH.Con -> [TH.Type]
responseConstructorFieldTypes = \case
    TH.NormalC _ fields -> map snd fields
    TH.RecC _ fields -> [fieldType | (_, _, fieldType) <- fields]
    TH.InfixC (_, leftType) _ (_, rightType) -> [leftType, rightType]
    TH.ForallC _ _ constructor -> responseConstructorFieldTypes constructor
    TH.GadtC _ fields _ -> map snd fields
    TH.RecGadtC _ fields _ -> [fieldType | (_, _, fieldType) <- fields]

commaSeparatedNames :: [Name] -> String
commaSeparatedNames names =
    Text.unpack (Text.intercalate ", " (map (Text.pack . TH.nameBase) names))

singleTypedRouteResponseHandlerExp :: TH.Type -> TypedRouteMetadata -> TH.Exp
singleTypedRouteResponseHandlerExp responseTy metadata =
    typedRouteResponseHandlerBase responseTy responseTy "success" (routeSuccessStatusCode metadata) (routeSuccessResponseDescription metadata) (not (routePrivate metadata)) (TH.ConE 'Just)

typedRouteResponseHandlerExp :: TH.Type -> TypedRouteMetadata -> RouteResponseSpec -> TH.Exp
typedRouteResponseHandlerExp responseTy metadata RouteResponseSpec{responseSpecConstructorName, responseSpecConstructorText, responseSpecViewType, responseSpecStatusCode, responseSpecDescription} =
    typedRouteResponseHandlerBase responseTy responseSpecViewType responseSpecConstructorText responseSpecStatusCode responseSpecDescription (not (routePrivate metadata)) (constructorMatcherExp responseSpecConstructorName)

typedRouteResponseHandlerBase :: TH.Type -> TH.Type -> Text -> Int -> Text -> Bool -> TH.Exp -> TH.Exp
typedRouteResponseHandlerBase responseTy viewTy responseName statusCode description publicRoute matcher =
    foldl
        TH.AppE
        ( TH.AppTypeE
            (TH.AppTypeE (TH.VarE 'mkTypedRouteResponseHandler) responseTy)
            viewTy
        )
        [ textExp responseName
        , statusExpFrom statusCode description
        , documentedExpectationExp publicRoute viewTy
        , matcher
        ]

constructorMatcherExp :: Name -> TH.Exp
constructorMatcherExp constructorName =
    let responseName = TH.mkName "_typedResponse"
        viewName = TH.mkName "_typedView"
     in TH.LamE
            [TH.VarP responseName]
            ( TH.CaseE
                (TH.VarE responseName)
                [ TH.Match
                    (TH.ConP constructorName [] [TH.VarP viewName])
                    (TH.NormalB (TH.AppE (TH.ConE 'Just) (TH.VarE viewName)))
                    []
                , TH.Match TH.WildP (TH.NormalB (TH.ConE 'Nothing)) []
                ]
            )

documentedExpectationExp :: Bool -> TH.Type -> TH.Exp
documentedExpectationExp publicRoute viewTy =
    if publicRoute
        then
            TH.AppE
                (TH.ConE 'Just)
                (TH.AppTypeE (TH.VarE 'typedDocumentedRenderExpectationForResponse) viewTy)
        else TH.ConE 'Nothing

typedRouteResponseDocumentExp :: Text -> TH.Type -> Int -> Text -> TH.Exp
typedRouteResponseDocumentExp responseName responseTy statusCode description =
    foldl
        TH.AppE
        (TH.AppTypeE (TH.ConE 'TypedRouteResponseDocument) responseTy)
        [ textExp responseName
        , statusExpFrom statusCode description
        , textExp description
        , TH.SigE (TH.ConE 'Proxy) (TH.AppT (TH.ConT ''Proxy) responseTy)
        ]

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

methodsListExp :: [StdMethod] -> TH.Exp
methodsListExp methods =
    TH.ListE [TH.ConE (TH.mkName (show method)) | method <- methods]

textExp :: Text -> TH.Exp
textExp value =
    TH.AppE (TH.VarE 'Text.pack) (TH.LitE (TH.StringL (Text.unpack value)))

maybeTextExp :: Maybe Text -> TH.Exp
maybeTextExp = \case
    Nothing -> TH.ConE 'Nothing
    Just value -> TH.AppE (TH.ConE 'Just) (textExp value)

statusExpFrom :: Int -> Text -> TH.Exp
statusExpFrom statusCode description =
    TH.AppE
        ( TH.AppE
            (TH.VarE 'mkStatus)
            (TH.LitE (TH.IntegerL (fromIntegral statusCode)))
        )
        ( TH.SigE
            (TH.LitE (TH.StringL (Text.unpack description)))
            (TH.ConT ''ByteString)
        )

gadtControllerConstraints :: ControllerInfo -> [ValidatedRoute] -> Q [TH.Type]
gadtControllerConstraints ctrl routesForController =
    concat <$> traverse (gadtRouteConstraints ctrl) routesForController

gadtRouteConstraints :: ControllerInfo -> ValidatedRoute -> Q [TH.Type]
gadtRouteConstraints ctrl route = do
    (bodyTy, responseTy) <- actionBodyResponse ctrl (coResultType (vrCon route))
    metadata <- typedRouteMetadata route
    responseTypes <-
        case routeResponses metadata of
            [] -> pure [responseTy]
            _ -> map responseSpecViewType <$> typedRouteResponseSpecs ctrl route responseTy metadata
    let actionTy = coResultType (vrCon route)
        publicRoute = not (routePrivate metadata)
        controllerConstraints =
            [ TH.AppT (TH.ConT ''TypedController) (TH.ConT (ciTypeName ctrl))
            , TH.AppT (TH.ConT ''Typeable) actionTy
            , TH.AppT (TH.ConT ''DecodeRequest) bodyTy
            ]
                <> map renderTypedResponseConstraint responseTypes
        typedDocumentConstraints =
            if publicRoute
                then TH.AppT (TH.ConT ''BuildTypedRequestBodyDoc) bodyTy : concatMap documentedResponseConstraints responseTypes
                else []
        pathConstraints =
            [ routeValueConstraints publicRoute captureType
            | segment <- vrPath route
            , captureType <- case segment of
                VSCapture _ ty -> [ty]
                VSSplat _ ty -> [ty]
                VSLiteral _ -> []
            ]
        queryConstraints =
            [ queryValueConstraints publicRoute innerType
                <> if publicRoute
                    then schemaValueConstraints (queryParameterSchemaType route fieldName kind innerType)
                    else []
            | (_urlName, fieldName, kind, innerType) <- vrQueryFields route
            ]
    pure (controllerConstraints <> typedDocumentConstraints <> concat pathConstraints <> concat queryConstraints)

renderTypedResponseConstraint :: TH.Type -> TH.Type
renderTypedResponseConstraint responseTy =
    TH.AppT (TH.ConT ''RenderTypedResponse) responseTy

documentedResponseConstraints :: TH.Type -> [TH.Type]
documentedResponseConstraints responseTy =
    [ renderTypedResponseConstraint responseTy
    , TH.AppT (TH.ConT ''ToSchema) (TH.AppT (TH.ConT ''ViewSupport.JsonResponse) responseTy)
    ]

routeValueConstraints :: Bool -> TH.Type -> [TH.Type]
routeValueConstraints includeSchema valueType =
    [TH.AppT (TH.ConT ''UrlCapture) valueType]
        <> schemaConstraints includeSchema valueType

queryValueConstraints :: Bool -> TH.Type -> [TH.Type]
queryValueConstraints includeSchema valueType =
    [TH.AppT (TH.ConT ''UrlCapture) valueType]
        <> schemaConstraints includeSchema valueType

schemaConstraints :: Bool -> TH.Type -> [TH.Type]
schemaConstraints includeSchema valueType =
    if includeSchema
        then
            [ TH.AppT (TH.ConT ''ToSchema) valueType
            , TH.AppT (TH.ConT ''Typeable) valueType
            ]
        else []

schemaValueConstraints :: TH.Type -> [TH.Type]
schemaValueConstraints valueType =
    [ TH.AppT (TH.ConT ''ToSchema) valueType
    , TH.AppT (TH.ConT ''Typeable) valueType
    ]

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
