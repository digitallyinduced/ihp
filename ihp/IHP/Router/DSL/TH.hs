{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Router.DSL.TH
Description: Template Haskell splice for the @routes@ quasi-quoter

The splice parses the DSL body, 'reify's the target controller type, and
emits 'HasPath' and 'CanRoute' instances driven by the parsed routes.

Cross-module references to names defined in "IHP.RouterSupport" and
"IHP.Router.UrlGenerator" ('HasPath', 'pathTo', 'CanRoute', 'runAction'',
etc.) are made via 'TH.mkName' to avoid a cyclic import. Those names must
be in scope at the splice call site — normally via @import IHP.RouterPrelude@.

Names from lower-level modules ('LiteralSeg', 'CaptureSeg', 'buildRouteTrie',
etc.) are referenced hygienically via 'TH.QuoteName' syntax.
-}
module IHP.Router.DSL.TH
    ( routes
    , routesDec
    ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Dec, Exp, Name, Pat, Type)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.Meta.Parse as Meta
import Data.Typeable (Typeable)

import IHP.Router.DSL.AST hiding (routes)
import qualified IHP.Router.DSL.AST as AST
import qualified IHP.Router.DSL.Parser as Parser
import IHP.Router.Trie (PatternSegment (..))
import IHP.Router.DSL.Runtime (buildRouteTrie, captureSpec, requireCapture)

-- | The @[routes|...|]@ quasi-quoter. Captures use RFC 6570 URI-template
-- syntax — @{name}@ for a single segment, @{+name}@ for a splat.
--
-- > [routes|PostsController
-- > GET    /posts              PostsAction
-- > GET    /posts/{postId}     ShowPostAction
-- > PATCH  /posts/{postId}     UpdatePostAction
-- > |]
--
-- Three header forms — all top-level declarations:
--
-- (1) __Binding-named__ (multi-controller). Lowercase-initial header
--     becomes the name of a 'FrontController.controllers'-ready binding.
--     The splice also emits 'HasPath' + 'CanRoute' for each reified
--     parent type.
--
-- > [routes|webRoutes
-- > GET /posts             PostsIndexAction
-- > GET /users             UsersIndexAction
-- > |]
-- >
-- > instance FrontController WebApplication where
-- >     controllers = webRoutes
--
-- (2) __Single-controller__. Uppercase-initial header = controller type.
--
-- (3) __No header__. Multi-controller, instances only (no binding).
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

-- | The expression-form splice: returns a @[ControllerRoute app]@
-- | The underlying splice function, exposed for callers that want to
-- provide a DSL body programmatically.
--
-- Header dispatch:
--
-- * @Just n@ starting uppercase — single-controller form; emit @HasPath@
--   + @CanRoute@ for controller type @n@.
-- * @Just n@ starting lowercase — binding-named multi-controller form;
--   reify each action's parent type, emit instances per controller, and
--   emit a top-level binding named @n@ that lists
--   @parseRoute \@Ctrl@ for each referenced controller.
-- * @Nothing@ — plain multi-controller form; emit instances only, no binding.
routesDec :: String -> Q [Dec]
routesDec source = case Parser.parseRoutes (Text.pack source) of
    Left err -> fail (renderParseError err)
    Right parsed -> case controllerName parsed of
        Just name
            | startsWithUpper name -> do
                ctrl <- reifyController name
                vs <- traverse (validateRoute ctrl) (AST.routes parsed)
                hasPathInst <- emitHasPath ctrl vs
                canRouteInst <- emitCanRoute ctrl vs
                pure [hasPathInst, canRouteInst]
            | otherwise -> do
                grouped <- groupRoutesByParent (AST.routes parsed)
                instanceDecs <- fmap concat $ traverse emitGroup grouped
                bindingDecs <- emitNamedBinding name (map fst grouped)
                pure (instanceDecs <> bindingDecs)
        Nothing -> do
            grouped <- groupRoutesByParent (AST.routes parsed)
            fmap concat $ traverse emitGroup grouped
  where
    startsWithUpper t = case Text.uncons t of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False

-- | Bucket routes by the parent type of their action constructor.
--
-- For each route, reify its action constructor's 'Name' to recover the
-- parent type's 'Name'. Routes targeting the same parent type are grouped
-- together; the resulting list preserves the first-seen order of
-- parent types so the generated instances line up with the DSL's visual
-- grouping.
groupRoutesByParent :: [Route] -> Q [(ControllerInfo, [Route])]
groupRoutesByParent rs = do
    -- Associate each route with its parent type 'Name'.
    withParents <- traverse (\r -> do p <- parentTypeOfAction r; pure (p, r)) rs
    -- Preserve first-seen order of parent types.
    let order = List.nub (map fst withParents)
    -- Reify each parent type once.
    ctrls <- traverse reifyControllerByName order
    let bucket p = [ r | (p', r) <- withParents, p' == p ]
    pure [ (ci, bucket (ciTypeName ci)) | ci <- ctrls ]

-- | Reify an action constructor 'Name' and return its parent type 'Name'.
parentTypeOfAction :: Route -> Q Name
parentTypeOfAction rt = do
    let conName = TH.mkName (Text.unpack (actionName (routeAction rt)))
    info <- TH.recover
        (fail $
            "routes (line " <> show (routeLine rt) <> "): " <>
            "cannot find action constructor '" <>
            Text.unpack (actionName (routeAction rt)) <>
            "' in scope. Is its module imported?")
        (TH.reify conName)
    case info of
        TH.DataConI _ _ parent -> pure parent
        _ -> fail $
            "routes (line " <> show (routeLine rt) <> "): '" <>
            Text.unpack (actionName (routeAction rt)) <>
            "' is not a data constructor"

-- | Reify a controller type that was discovered via its action constructor
-- (so we already know a valid 'Name' — no need for name-lookup recovery).
reifyControllerByName :: Name -> Q ControllerInfo
reifyControllerByName tyName = do
    info <- TH.reify tyName
    cs <- case info of
        TH.TyConI (TH.DataD _ _ _ _ cs _) -> traverse extractCon cs
        TH.TyConI (TH.NewtypeD _ _ _ _ c _) -> fmap (: []) (extractCon c)
        _ -> fail $
            "routes: '" <> TH.nameBase tyName <>
            "' is not a data or newtype declaration"
    pure ControllerInfo
        { ciTypeName = tyName
        , ciConstructors = Map.fromList
            [ (Text.pack (TH.nameBase (coName c)), c) | c <- cs ]
        }
  where
    extractCon :: TH.Con -> Q ConstructorInfo
    extractCon = \case
        TH.RecC n flds -> pure ConstructorInfo
            { coName = n
            , coFields = Map.fromList
                [ (Text.pack (TH.nameBase fn), ty) | (fn, _, ty) <- flds ]
            }
        TH.NormalC n [] -> pure ConstructorInfo
            { coName = n, coFields = Map.empty }
        TH.NormalC n _ -> fail $
            "routes: constructor '" <> TH.nameBase n <>
            "' must use record syntax or be nullary"
        TH.ForallC _ _ inner -> extractCon inner
        c -> fail $ "routes: unsupported constructor form: " <> show (TH.ppr c)

-- | Emit instances for one controller group.
emitGroup :: (ControllerInfo, [Route]) -> Q [Dec]
emitGroup (ctrl, rs) = do
    vs <- traverse (validateRoute ctrl) rs
    hasPathInst <- emitHasPath ctrl vs
    canRouteInst <- emitCanRoute ctrl vs
    pure [hasPathInst, canRouteInst]

renderParseError :: Parser.ParseError -> String
renderParseError e =
    "routes: parse error on line "
        <> show (Parser.errorLine e)
        <> ": "
        <> Text.unpack (Parser.errorMessage e)

---------------------------------------------------------------------------
-- Reification
---------------------------------------------------------------------------

data ControllerInfo = ControllerInfo
    { ciTypeName     :: !Name
    , ciConstructors :: !(Map.Map Text ConstructorInfo)
    }

data ConstructorInfo = ConstructorInfo
    { coName   :: !Name
    , coFields :: !(Map.Map Text Type)
    }

reifyController :: Text -> Q ControllerInfo
reifyController name = do
    let tyName = TH.mkName (Text.unpack name)
    info <- TH.recover
        (fail $ "routes: cannot find type '" <> Text.unpack name <>
                "' in scope. Is it defined or imported?")
        (TH.reify tyName)
    cs <- case info of
        TH.TyConI (TH.DataD _ _ _ _ cs _) -> traverse extractCon cs
        TH.TyConI (TH.NewtypeD _ _ _ _ c _) -> fmap (: []) (extractCon c)
        _ -> fail $
            "routes: '" <> Text.unpack name <>
            "' is not a data or newtype declaration"
    pure ControllerInfo
        { ciTypeName = tyName
        , ciConstructors = Map.fromList
            [ (Text.pack (TH.nameBase (coName c)), c) | c <- cs ]
        }
  where
    extractCon :: TH.Con -> Q ConstructorInfo
    extractCon = \case
        TH.RecC n flds -> pure ConstructorInfo
            { coName = n
            , coFields = Map.fromList
                [ (Text.pack (TH.nameBase fn), ty) | (fn, _, ty) <- flds ]
            }
        TH.NormalC n [] -> pure ConstructorInfo
            { coName = n, coFields = Map.empty }
        TH.NormalC n _ -> fail $
            "routes: constructor '" <> TH.nameBase n <>
            "' must use record syntax or be nullary"
        TH.ForallC _ _ inner -> extractCon inner
        c -> fail $ "routes: unsupported constructor form: " <> show (TH.ppr c)

---------------------------------------------------------------------------
-- Validation
---------------------------------------------------------------------------

data ValidatedRoute = ValidatedRoute
    { vrMethods           :: ![Method]
    , vrPath              :: ![ValidatedSeg]
    , vrCon               :: !ConstructorInfo
    , vrLine              :: !Int
    , vrBindingsByCapture :: !(Map.Map Text Text)
    }

data ValidatedSeg
    = VSLiteral !Text
    | VSCapture !Text !Type
    | VSSplat   !Text !Type

validateRoute :: ControllerInfo -> Route -> Q ValidatedRoute
validateRoute ctrl rt = do
    con <- case Map.lookup (actionName (routeAction rt)) (ciConstructors ctrl) of
        Just c -> pure c
        Nothing -> fail $
            "routes (line " <> show (routeLine rt) <> "): " <>
            "action '" <> Text.unpack (actionName (routeAction rt)) <>
            "' is not a constructor of " <> TH.nameBase (ciTypeName ctrl) <>
            ". Known: " <>
            List.intercalate ", " (map Text.unpack (Map.keys (ciConstructors ctrl)))
    let bindingsByCapture = Map.fromList
            [ (captureN, fieldN)
            | (fieldN, captureN) <- fieldBindings (routeAction rt)
            ]
    segs <- traverse (resolveSeg rt con bindingsByCapture) (routePath rt)
    pure ValidatedRoute
        { vrMethods = routeMethods rt
        , vrPath = segs
        , vrCon = con
        , vrLine = routeLine rt
        , vrBindingsByCapture = bindingsByCapture
        }

resolveSeg
    :: Route
    -> ConstructorInfo
    -> Map.Map Text Text
    -> PathSeg
    -> Q ValidatedSeg
resolveSeg rt con bindingsByCapture = \case
    Literal t -> pure (VSLiteral t)
    Capture capName tyAnn -> do
        ty <- resolveCaptureType rt con bindingsByCapture capName tyAnn
        pure (VSCapture capName ty)
    Splat capName tyAnn -> do
        ty <- case tyAnn of
            Just raw -> parseTypeAnn rt raw
            Nothing  -> pure (TH.ConT ''Text)
        pure (VSSplat capName ty)

resolveCaptureType
    :: Route -> ConstructorInfo -> Map.Map Text Text -> Text -> Maybe Text -> Q Type
resolveCaptureType rt con bindingsByCapture capName = \case
    Just raw -> parseTypeAnn rt raw
    Nothing -> do
        let fieldName = Map.findWithDefault capName capName bindingsByCapture
        case Map.lookup fieldName (coFields con) of
            Just ty -> pure ty
            Nothing -> fail $
                "routes (line " <> show (routeLine rt) <> "): " <>
                "capture '#" <> Text.unpack capName <>
                "' has no matching field on " <> TH.nameBase (coName con) <>
                ". Known fields: " <>
                List.intercalate ", " (map Text.unpack (Map.keys (coFields con)))

parseTypeAnn :: Route -> Text -> Q Type
parseTypeAnn rt raw = case Meta.parseType (Text.unpack raw) of
    Right ty -> pure ty
    Left err -> fail $
        "routes (line " <> show (routeLine rt) <> "): " <>
        "invalid type annotation '" <> Text.unpack raw <> "': " <> err

---------------------------------------------------------------------------
-- Cross-module TH names (resolved at splice use-site)
---------------------------------------------------------------------------

hasPathClass, pathToFn, renderCaptureFn :: Name
hasPathClass    = TH.mkName "HasPath"
pathToFn        = TH.mkName "pathTo"
renderCaptureFn = TH.mkName "renderCapture"

canRouteClass, parseRoutePrimeFn, toControllerRouteFn :: Name
canRouteClass       = TH.mkName "CanRoute"
parseRoutePrimeFn   = TH.mkName "parseRoute'"
toControllerRouteFn = TH.mkName "toControllerRoute"

controllerRouteTrieCon, runActionPrimeFn :: Name
controllerRouteTrieCon = TH.mkName "ControllerRouteTrie"
runActionPrimeFn       = TH.mkName "runAction'"

stdMethodCon :: Method -> Name
stdMethodCon m = TH.mkName (Text.unpack (methodToText m))

---------------------------------------------------------------------------
-- Code generation — HasPath
---------------------------------------------------------------------------

emitHasPath :: ControllerInfo -> [ValidatedRoute] -> Q Dec
emitHasPath ctrl vs = do
    let byCon = Map.fromListWith (\_ first -> first)
            [ (Text.pack (TH.nameBase (coName (vrCon r))), r) | r <- vs ]
    clauses <- traverse emitPathClause (Map.elems byCon)
    let fallback = TH.Clause
            [TH.VarP (TH.mkName "_action")]
            (TH.NormalB
                (TH.AppE (TH.VarE 'error)
                    (TH.LitE (TH.StringL
                        ("pathTo: no route covers this constructor of "
                            <> TH.nameBase (ciTypeName ctrl))))))
            []
    pure (TH.InstanceD Nothing []
        (TH.AppT (TH.ConT hasPathClass) (TH.ConT (ciTypeName ctrl)))
        [TH.FunD pathToFn (clauses <> [fallback])])

emitPathClause :: ValidatedRoute -> Q TH.Clause
emitPathClause vr = do
    let captureFields :: [Text]
        captureFields =
            [ n | VSCapture n _ <- vrPath vr ]
                <> [ n | VSSplat n _ <- vrPath vr ]
        conName = coName (vrCon vr)
        pat :: Pat
        pat = case captureFields of
            [] -> TH.ConP conName [] []
            _  -> TH.RecP conName
                [ ( fieldNameFor vr capName
                  , TH.VarP (TH.mkName (Text.unpack capName))
                  )
                | capName <- captureFields
                ]
    let body = pathExpr (vrPath vr)
    pure (TH.Clause [pat] (TH.NormalB body) [])

fieldNameFor :: ValidatedRoute -> Text -> Name
fieldNameFor vr capName =
    let fieldName = Map.findWithDefault capName capName (vrBindingsByCapture vr)
     in TH.mkName (Text.unpack fieldName)

pathExpr :: [ValidatedSeg] -> Exp
pathExpr = \case
    [] -> TH.LitE (TH.StringL "/")
    segs ->
        let pieces = concatMap segPieces segs
         in foldr1 (\a b -> TH.InfixE (Just a) (TH.VarE '(<>)) (Just b)) pieces
  where
    segPieces :: ValidatedSeg -> [Exp]
    segPieces = \case
        VSLiteral t ->
            [ TH.LitE (TH.StringL "/")
            , TH.LitE (TH.StringL (Text.unpack t))
            ]
        VSCapture n _ ->
            [ TH.LitE (TH.StringL "/")
            , TH.AppE (TH.VarE renderCaptureFn)
                (TH.VarE (TH.mkName (Text.unpack n)))
            ]
        VSSplat n _ ->
            [ TH.LitE (TH.StringL "/")
            , TH.AppE (TH.VarE renderCaptureFn)
                (TH.VarE (TH.mkName (Text.unpack n)))
            ]

---------------------------------------------------------------------------
-- Code generation — CanRoute + trie
---------------------------------------------------------------------------

emitCanRoute :: ControllerInfo -> [ValidatedRoute] -> Q Dec
emitCanRoute ctrl vs = do
    trieE <- emitTrieFragment vs
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
                    (TH.AppE (TH.ConE controllerRouteTrieCon) trieE))
                []]
    pure (TH.InstanceD Nothing []
        (TH.AppT (TH.ConT canRouteClass) (TH.ConT (ciTypeName ctrl)))
        [parseRouteDecl, toControllerRouteDecl])

emitTrieFragment :: [ValidatedRoute] -> Q Exp
emitTrieFragment vs = do
    entries <- traverse emitEntriesPerRoute vs
    let listE = TH.ListE (concat entries)
    pure (TH.AppE (TH.VarE 'buildRouteTrie) listE)

emitEntriesPerRoute :: ValidatedRoute -> Q [Exp]
emitEntriesPerRoute vr = traverse (emitOneEntry vr) (vrMethods vr)

emitOneEntry :: ValidatedRoute -> Method -> Q Exp
emitOneEntry vr method = do
    handlerE <- handlerExpr vr
    pure $ TH.TupE
        [ Just (TH.ConE (stdMethodCon method))
        , Just (patternExp (vrPath vr))
        , Just handlerE
        ]

patternExp :: [ValidatedSeg] -> Exp
patternExp = TH.ListE . map segExp
  where
    segExp :: ValidatedSeg -> Exp
    segExp = \case
        VSLiteral t ->
            TH.AppE (TH.ConE 'LiteralSeg)
                (TH.LitE (TH.StringL (Text.unpack t)))
        VSCapture n ty ->
            TH.AppE (TH.ConE 'CaptureSeg) (captureSpecE n ty)
        VSSplat n _ ->
            TH.AppE (TH.ConE 'SplatSeg)
                (TH.LitE (TH.StringL (Text.unpack n)))

    captureSpecE :: Text -> Type -> Exp
    captureSpecE n ty =
        TH.AppE
            (TH.AppTypeE (TH.VarE 'captureSpec) ty)
            (TH.LitE (TH.StringL (Text.unpack n)))

-- | Build the @Captures -> Application@ handler for one route.
handlerExpr :: ValidatedRoute -> Q Exp
handlerExpr vr = do
    let captureFields = [ (n, ty) | VSCapture n ty <- vrPath vr ]
                     <> [ (n, ty) | VSSplat n ty <- vrPath vr ]
        capturesName = TH.mkName "_dslCaptures"
    constructed <- constructAction vr captureFields capturesName
    pure $ TH.LamE
        [TH.VarP capturesName]
        (TH.AppE (TH.VarE runActionPrimeFn) constructed)

-- | Build the action constructor application with fields filled from captures.
constructAction
    :: ValidatedRoute
    -> [(Text, Type)]
    -> Name
    -> Q Exp
constructAction vr fields capturesName =
    case fields of
        [] -> pure (TH.ConE (coName (vrCon vr)))
        _  -> do
            let bind (capName, ty) =
                    ( fieldNameFor vr capName
                    , TH.AppE
                        (TH.AppE
                            (TH.AppTypeE (TH.VarE 'requireCapture) ty)
                            (TH.LitE (TH.StringL (Text.unpack capName))))
                        (TH.VarE capturesName)
                    )
            pure (TH.RecConE (coName (vrCon vr)) (map bind fields))

---------------------------------------------------------------------------
-- Code generation — named binding for lowercase-header form
---------------------------------------------------------------------------

-- | Emit a top-level binding named by the user. Given the header
-- @webRoutes@ and controllers @[PostsController, UsersController]@:
--
-- > webRoutes = [ parseRoute @PostsController, parseRoute @UsersController ]
--
-- The binding is polymorphic in the application type; when splatted into
-- 'FrontController.controllers' for a concrete app, GHC infers the right
-- @app@.
--
-- @parseRoute@ carries implicit-parameter constraints ('?request',
-- '?respond', '?application', plus 'Controller', 'CanRoute',
-- 'InitControllerContext', and 'Typeable') that GHC cannot abstract at
-- the use site, so we emit an explicit signature enumerating all of them.
emitNamedBinding :: Text -> [ControllerInfo] -> Q [Dec]
emitNamedBinding bindingTxt ctrls = do
    let valName = TH.mkName (Text.unpack bindingTxt)
        appTyVarName = TH.mkName "app"
        appTy = TH.VarT appTyVarName
        parseRouteName = TH.mkName "parseRoute"
        entries =
            [ TH.AppTypeE (TH.VarE parseRouteName) (TH.ConT (ciTypeName c))
            | c <- ctrls
            ]
        bindingExp = TH.ListE entries

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
        ctx = implicitReqTy : implicitResTy : implicitAppTy
            : initContextTy : typeableAppTy
            : concatMap perCtrl ctrls
        resultTy = TH.AppT TH.ListT
            (TH.AppT (TH.ConT (TH.mkName "ControllerRoute")) appTy)
        bindingTy = TH.ForallT [TH.PlainTV appTyVarName TH.SpecifiedSpec] ctx resultTy

    pure
        [ TH.SigD valName bindingTy
        , TH.FunD valName [TH.Clause [] (TH.NormalB bindingExp) []]
        ]
