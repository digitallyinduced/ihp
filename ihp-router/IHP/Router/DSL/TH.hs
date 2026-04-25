{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Router.DSL.TH
Description: Template Haskell splice for the @routes@ quasi-quoter

The splice parses the DSL body, 'reify's the target controller type, and
emits IHP-free declarations:

* @instance HasPath Ctrl@ — pathTo per constructor.
* @\<ctrlLower>Trie :: (Ctrl -> Application) -> RouteTrie@ — top-level
  binding that builds the trie when applied to a user-supplied dispatch
  function. Plug it into 'IHP.Router.Middleware.routeTrieMiddleware' for
  a working WAI dispatcher.

The IHP-flavoured @[routes|…|]@ quoter (which additionally emits an
@instance CanRoute Ctrl@ and a @webRoutes :: [ControllerRoute app]@
binding) lives in IHP's @IHP.Router.IHP@ module and calls into
'parseAndReify' \/ 'genericEmit' here.

Names referenced from inside the splice ('HasPath', 'pathTo',
'renderCapture', 'parseCapture') are imported via 'TH.mkName' so they
resolve at the splice call site — typically via @import IHP.Router.WAI@
for plain WAI users or @import IHP.RouterPrelude@ for IHP apps.

Names from sibling @ihp-router@ modules ('LiteralSeg', 'CaptureSeg',
'buildRouteTrie', 'mkHandler', 'mkHandlerQ', …) are referenced
hygienically via 'TH.QuoteName' syntax.
-}
module IHP.Router.DSL.TH
    ( -- * Splice entry points
      routes
    , routesDec
    , genericRoutesDec
      -- * Shared types and helpers
      --
      -- | Re-exported for the IHP-side @IHP.Router.IHP@ shim, which
      -- composes its own quoter on top of 'parseAndReify' \/
      -- 'genericEmit'. Plain WAI users typically don't need to touch
      -- these directly.
    , parseAndReify
    , genericEmit
    , ParsedBlock (..)
    , HeaderForm (..)
    , ControllerInfo (..)
    , ConstructorInfo (..)
    , ValidatedRoute (..)
    , ValidatedSeg (..)
    , QueryFieldKind (..)
    , trieValueName
    ) where

import Prelude
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q, Dec, Exp, Name, Pat, Type)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.Meta.Parse as Meta
import qualified Data.Set as Set
import Network.HTTP.Types.Method (StdMethod (..))

import IHP.Router.DSL.AST hiding (routes)
import qualified IHP.Router.DSL.AST as AST
import qualified IHP.Router.DSL.Parser as Parser
import IHP.Router.Trie (PatternSegment (..))
import IHP.Router.DSL.Runtime
    ( buildRouteTrie
    , captureSpec
    , mkHandler
    , mkHandlerQ
    , queryParamRequired
    , queryParamOptional
    , queryParamList
    , renderQueryString
    )

-- | The @[routes|...|]@ quasi-quoter. Captures use RFC 6570 URI-template
-- syntax — @{name}@ for a single segment, @{+name}@ for a splat — and
-- @?name1&name2@ for query-string parameters.
--
-- > [routes|PostsController
-- > GET    /posts                 PostsAction
-- > GET    /posts/{postId}        ShowPostAction
-- > PATCH  /posts/{postId}        UpdatePostAction
-- > GET    /search?q&page         SearchAction
-- > |]
--
-- __Query parameters__ are declared explicitly: each @?name@ names a
-- record field on the action constructor that the URL carries in the
-- query string. The field's Haskell type determines the shape:
-- plain @a@ is required (missing/unparseable ⇒ 404), @'Maybe' a@ is
-- optional, @[a]@ collects repeated values (@?tags=a&tags=b@).
-- __Every record field of the action constructor must be covered__
-- by either a path capture or a query-param entry — unbound fields
-- fail at splice time with a pointer to the exact fields left over.
--
-- __Renaming.__ To map a capture or query-param name to a differently
-- named record field, use the @{ field = #name }@ syntax after the
-- action. Works for path captures and query params alike:
--
-- > GET /ShowPost?id     ShowPostAction { postId = #id }
-- > GET /users/{uid}     ShowUserAction { userId = #uid }
--
-- This is the @ihp-router@ (plain-WAI) flavour of the quoter — emits
-- 'HasPath' instances and a per-controller @\<ctrlLower>Trie@ binding
-- only. IHP apps use the wrapping quoter from "IHP.Router.IHP" (re-exported
-- through "IHP.Router.DSL"), which additionally emits the IHP-flavoured
-- @CanRoute@ instance and lowercase-header @webRoutes@ binding.
--
-- Three header forms — all top-level declarations:
--
-- (1) __Single-controller__. Uppercase-initial header = controller type.
-- (2) __Binding-named__ (multi-controller). Lowercase-initial header is
--     the name of a binding the splice emits. In @ihp-router@ it carries
--     the per-controller trie values; in IHP it additionally carries the
--     @[ControllerRoute app]@ list for @FrontController.controllers@.
-- (3) __No header__. Multi-controller, instances + trie values only.
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

-- | The default splice for the @[routes|…|]@ quoter in @ihp-router@.
-- Emits the IHP-free declarations a plain WAI app needs — an
-- @instance HasPath Ctrl@ per controller and a parameterised
-- @\<ctrlLower>Trie :: (Ctrl -> Application) -> RouteTrie@ binding
-- per controller.
--
-- IHP apps don't call this directly; they use the IHP-flavoured
-- @routes@ quoter from "IHP.Router.IHP", which composes 'genericEmit'
-- with its own IHP-specific 'CanRoute' \/ @webRoutes@ emission.
routesDec :: String -> Q [Dec]
routesDec = genericRoutesDec

-- | The IHP-free entry point: parses the DSL, reifies the
-- controller(s), and emits 'genericEmit''s output. Suitable for plain
-- WAI applications.
--
-- Same as 'routesDec'; provided under the more descriptive name so the
-- IHP-side shim can call it explicitly when composing the
-- IHP-flavoured quoter.
genericRoutesDec :: String -> Q [Dec]
genericRoutesDec source = parseAndReify source >>= genericEmit

---------------------------------------------------------------------------
-- Shared parse + reify
---------------------------------------------------------------------------

-- | A parsed @[routes|…|]@ body, ready for emission. Produced by
-- 'parseAndReify' and consumed by 'genericEmit' and 'ihpEmit'.
data ParsedBlock = ParsedBlock
    { pbHeader :: !HeaderForm
        -- ^ Which header shape the user wrote.
    , pbGroups :: ![(ControllerInfo, [ValidatedRoute])]
        -- ^ Routes grouped by parent controller type, in the order
        -- those types first appear in the DSL block. For
        -- single-controller uppercase blocks this list has length 1.
    }

-- | Which of the three header forms the parser saw.
data HeaderForm
    = HeaderUppercase !Text
        -- ^ @Just name@ with an uppercase initial — single-controller
        -- form. The emitters skip @groupRoutesByParent@ and treat the
        -- single entry in 'pbGroups' as the user's one target type.
    | HeaderLowercase !Text
        -- ^ @Just name@ with a lowercase initial — multi-controller
        -- binding-named form. The IHP emitter additionally produces a
        -- top-level @name :: [ControllerRoute app]@ binding.
    | HeaderAbsent
        -- ^ Header-less multi-controller form. Instances only, no binding.

-- | Parse the DSL body, reify each referenced controller type, and
-- validate every route against its action constructor. Returns a
-- 'ParsedBlock' both emitters can consume.
parseAndReify :: String -> Q ParsedBlock
parseAndReify source = case Parser.parseRoutes (Text.pack source) of
    Left err -> fail (renderParseError err)
    Right parsed -> do
        (header, groups) <- case controllerName parsed of
            Just name
                | startsWithUpper name -> do
                    ctrl <- reifyController name
                    vs <- traverse (validateRoute ctrl) (AST.routes parsed)
                    pure (HeaderUppercase name, [(ctrl, vs)])
                | otherwise -> do
                    grouped <- groupRoutesByParent (AST.routes parsed)
                    validated <- traverse
                        (\(ctrl, rs) -> do vs <- traverse (validateRoute ctrl) rs; pure (ctrl, vs))
                        grouped
                    pure (HeaderLowercase name, validated)
            Nothing -> do
                grouped <- groupRoutesByParent (AST.routes parsed)
                validated <- traverse
                    (\(ctrl, rs) -> do vs <- traverse (validateRoute ctrl) rs; pure (ctrl, vs))
                    grouped
                pure (HeaderAbsent, validated)
        pure ParsedBlock { pbHeader = header, pbGroups = groups }
  where
    startsWithUpper t = case Text.uncons t of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False

---------------------------------------------------------------------------
-- Generic emission (→ future ihp-router)
---------------------------------------------------------------------------

-- | Emit the generic 'HasPath' instance per controller plus the
-- top-level @\<ctrlLower>Trie@ binding per controller. No IHP-specific
-- references — this is the half that moves to @ihp-router@ in the
-- extraction PR.
genericEmit :: ParsedBlock -> Q [Dec]
genericEmit ParsedBlock { pbGroups } = do
    hasPathDecs <- traverse (\(ctrl, vs) -> emitHasPath ctrl vs) pbGroups
    trieValueDecs <- fmap concat $ traverse emitTrieValue pbGroups
    pure (hasPathDecs <> trieValueDecs)

---------------------------------------------------------------------------
-- IHP-specific emission (stays in IHP)
---------------------------------------------------------------------------

-- | Emit the per-controller top-level binding @\<ctrlLower>Trie@. The
-- inferred type is @(Ctrl -> Application) -> RouteTrie@; the body is
-- @\\dispatchFn -> buildRouteTrie [(methods, pattern, mkHandler dispatchFn …) … ]@.
--
-- Plain WAI users call this binding with their own dispatch function;
-- the IHP shim's 'emitCanRoute' calls it with @runAction'@ inside the
-- @CanRoute@ instance.
--
-- No explicit signature is emitted: GHC infers the right type from
-- the lambda body, and an explicit signature would force the user's
-- @IHP.RouterPrelude@ to re-export 'Network.Wai.Application' and
-- 'IHP.Router.Trie.RouteTrie' (which it doesn't today).
emitTrieValue :: (ControllerInfo, [ValidatedRoute]) -> Q [Dec]
emitTrieValue (ctrl, vs) = do
    dispatchFnName <- TH.newName "dispatchFn"
    trieE <- emitTrieFragment dispatchFnName vs
    let valName = trieValueName (ciTypeName ctrl)
        valBody = TH.LamE [TH.VarP dispatchFnName] trieE
    pure
        [ TH.FunD valName [TH.Clause [] (TH.NormalB valBody) []]
        ]

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
        TH.RecC n flds ->
            let ordered = [ (Text.pack (TH.nameBase fn), ty) | (fn, _, ty) <- flds ]
             in pure ConstructorInfo
                    { coName        = n
                    , coFields      = Map.fromList ordered
                    , coFieldsOrder = ordered
                    }
        TH.NormalC n [] -> pure ConstructorInfo
            { coName = n, coFields = Map.empty, coFieldsOrder = [] }
        TH.NormalC n _ -> fail $
            "routes: constructor '" <> TH.nameBase n <>
            "' must use record syntax or be nullary"
        TH.ForallC _ _ inner -> extractCon inner
        c -> fail $ "routes: unsupported constructor form: " <> show (TH.ppr c)

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
    { coName        :: !Name
    , coFields      :: !(Map.Map Text Type)
    -- ^ Fast lookup by name.
    , coFieldsOrder :: ![(Text, Type)]
    -- ^ Same entries as 'coFields' but in the record-declaration order.
    -- Query-string rendering uses this to mirror AutoRoute's behaviour
    -- of producing URL params in the order fields were declared.
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
        TH.RecC n flds ->
            let ordered = [ (Text.pack (TH.nameBase fn), ty) | (fn, _, ty) <- flds ]
             in pure ConstructorInfo
                    { coName        = n
                    , coFields      = Map.fromList ordered
                    , coFieldsOrder = ordered
                    }
        TH.NormalC n [] -> pure ConstructorInfo
            { coName = n, coFields = Map.empty, coFieldsOrder = [] }
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
    -- ^ Query-param bindings declared explicitly in the route's @?name&…@
    -- suffix. Each entry is @(urlName, fieldName, kind, innerType)@:
    --
    -- * __urlName__ is what the user wrote in the DSL's @?urlName@
    --   suffix — it becomes the query-string key in both the rendered
    --   URL (@pathTo@) and the lookup at request-handling time.
    -- * __fieldName__ is the record field the urlName binds to. They
    --   usually match; they differ only when the user wrote a
    --   @{ field = #urlName }@ rebinding in the 'ActionRef'.
    -- * __kind__ is 'QFRequired' for plain fields (@a@), 'QFOptional'
    --   for @'Maybe' a@ fields (absent query params decode to 'Nothing'),
    --   or 'QFList' for @[a]@ fields (0+ repeated values).
    -- * __innerType__ is the unwrapped field type (strips one level of
    --   @Maybe@ or @[]@) used for 'parseCapture' / 'renderCapture'.
    --
    -- The order mirrors the order in the DSL so @pathTo@'s rendered query
    -- string reads left-to-right as the user wrote it.
    , vrQueryFields       :: ![(Text, Text, QueryFieldKind, Type)]
    }

data ValidatedSeg
    = VSLiteral !Text
    | VSCapture !Text !Type
    | VSSplat   !Text !Type

data QueryFieldKind
    = QFRequired  -- ^ plain @a@; 'Nothing' from decoder → 404
    | QFOptional  -- ^ @'Maybe' a@; absent → 'Nothing', present-parseable → @'Just' v@
    | QFList      -- ^ @[a]@; 0+ repeated query values
    deriving (Eq, Show)

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

    -- Names bound by path captures (after applying any {field = #cap}
    -- override). Used both for duplicate-detection against the query
    -- list and for the field-coverage check below.
    let pathBoundFields :: Set.Set Text
        pathBoundFields = Set.fromList
            [ fieldNameForCapture bindingsByCapture capName
            | seg <- segs
            , capName <- pathSegCaptureName seg
            ]

    -- Resolve the explicit @?name1&name2@ list into (fieldName, kind,
    -- innerType) triples. Field names use the same binding override:
    -- @?id ShowThreadAction { threadId = #id }@ maps query param @id@
    -- to record field @threadId@.
    queryFields <- traverse (resolveQueryParam rt con bindingsByCapture pathBoundFields)
                            (routeQueryParams rt)

    -- Duplicate check: a urlName must not appear twice in the query
    -- list (would produce two pathTo entries for the same key and two
    -- handler decoders). Checked against the URL name, not the record
    -- field — the rebinding form {fieldA = #x, fieldB = #x} would
    -- otherwise slip through and render two @?x=…&x=…@ pairs.
    let duplicateUrlNames = findDuplicates (map fstOf4 queryFields)
    case duplicateUrlNames of
        [] -> pure ()
        (d : _) -> fail $
            "routes (line " <> show (routeLine rt) <> "): " <>
            "query parameter '?" <> Text.unpack d <>
            "' is declared more than once"

    -- Coverage check: every record field of the action constructor must
    -- be accounted for — either bound by a path capture or named in the
    -- query list. Unbound fields at runtime would leave pathTo's record
    -- pattern match incomplete and the handler's constructor call with
    -- missing fields.
    let queryBoundFields = Set.fromList [ fn | (_, fn, _, _) <- queryFields ]
        allBoundFields = pathBoundFields `Set.union` queryBoundFields
        missingFields =
            [ n | (n, _) <- coFieldsOrder con, not (Set.member n allBoundFields) ]
    case missingFields of
        [] -> pure ()
        (firstField : _) -> fail $
            "routes (line " <> show (routeLine rt) <> "): " <>
            "action '" <> TH.nameBase (coName con) <>
            "' has fields not covered by the route: " <>
            List.intercalate ", " (map Text.unpack missingFields) <>
            ".\nAdd each to the path (e.g. '/path/{" <>
            Text.unpack firstField <>
            "}') or the query list (e.g. '/path?" <>
            List.intercalate "&" (map Text.unpack missingFields) <> "')."

    pure ValidatedRoute
        { vrMethods = routeMethods rt
        , vrPath = segs
        , vrCon = con
        , vrLine = routeLine rt
        , vrBindingsByCapture = bindingsByCapture
        , vrQueryFields = queryFields
        }
  where
    fstOf4 (a, _, _, _) = a
    findDuplicates xs = go Set.empty xs
      where
        go _ [] = []
        go seen (x : rest)
            | Set.member x seen = x : go seen rest
            | otherwise         = go (Set.insert x seen) rest

-- | Resolve one query-param name from the DSL to a
-- @(urlName, fieldName, kind, innerType)@ tuple by looking it up in the
-- constructor's fields, applying any @{field = #cap}@ override, and
-- rejecting duplicates with path captures.
--
-- When the user writes @?id ShowPostAction { postId = #id }@:
--   urlName = "id", fieldName = "postId" — the URL key is @id@ (what
--   'pathTo' emits, what the handler looks up), the record field on
--   'ShowPostAction' is @postId@.
resolveQueryParam
    :: Route
    -> ConstructorInfo
    -> Map.Map Text Text      -- bindingsByCapture
    -> Set.Set Text           -- path-bound field names
    -> Text                   -- the ?name (urlName)
    -> Q (Text, Text, QueryFieldKind, Type)
resolveQueryParam rt con bindingsByCapture pathBound urlName = do
    let fieldName = fieldNameForCapture bindingsByCapture urlName
    case Map.lookup fieldName (coFields con) of
        Nothing -> fail $
            "routes (line " <> show (routeLine rt) <> "): " <>
            "query parameter '?" <> Text.unpack urlName <>
            "' has no matching field on " <> TH.nameBase (coName con) <>
            ". Known fields: " <>
            List.intercalate ", " (map Text.unpack (Map.keys (coFields con)))
        Just ty
            | Set.member fieldName pathBound -> fail $
                "routes (line " <> show (routeLine rt) <> "): " <>
                "field '" <> Text.unpack fieldName <>
                "' is bound by a path capture — remove it from the query list"
            | otherwise ->
                pure (urlName, fieldName, classifyQueryField ty, innerType ty)

-- | Name of the field a capture segment binds, applying any
-- @{field = #capture}@ override map.
fieldNameForCapture :: Map.Map Text Text -> Text -> Text
fieldNameForCapture bindings capName =
    Map.findWithDefault capName capName bindings

-- | Capture name carried by a path segment, if any (literals carry none).
pathSegCaptureName :: ValidatedSeg -> [Text]
pathSegCaptureName = \case
    VSLiteral _   -> []
    VSCapture n _ -> [n]
    VSSplat n _   -> [n]

-- | Classify a field type as required / optional / list for query-param
-- purposes. Unwraps one level of @Maybe@ or @[]@.
classifyQueryField :: Type -> QueryFieldKind
classifyQueryField = \case
    TH.AppT (TH.ConT n) _ | n == ''Maybe -> QFOptional
    TH.AppT TH.ListT _                   -> QFList
    _                                    -> QFRequired

-- | Unwrap a @Maybe a@ or @[a]@ to its inner @a@; identity for other
-- types. Used at emission time to pick the right 'parseCapture @a'
-- specialisation for the query-string decoder.
innerType :: Type -> Type
innerType = \case
    TH.AppT (TH.ConT n) inner | n == ''Maybe -> inner
    TH.AppT TH.ListT inner                   -> inner
    ty                                       -> ty

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
        queryFields = vrQueryFields vr
        conName = coName (vrCon vr)

        -- Pattern-match on all relevant fields: path captures get bound
        -- to their capture names; query fields get bound to their field
        -- names directly (so we can render them below).
        recordFieldsForPat :: [(Name, Pat)]
        recordFieldsForPat =
               [ (fieldNameFor vr capName, TH.VarP (TH.mkName (Text.unpack capName)))
               | capName <- captureFields
               ]
            <> [ (TH.mkName (Text.unpack fieldName), TH.VarP (TH.mkName (Text.unpack fieldName)))
               | (_, fieldName, _, _) <- queryFields
               ]

        pat :: Pat
        pat = case recordFieldsForPat of
            [] -> TH.ConP conName [] []
            _  -> TH.RecP conName recordFieldsForPat

    let body = buildPathAndQueryExpr vr
    pure (TH.Clause [pat] (TH.NormalB body) [])

-- | Render a route's @pathTo@ body: path segments concatenated, with a
-- query-string suffix appended if the constructor has any unbound fields.
buildPathAndQueryExpr :: ValidatedRoute -> Exp
buildPathAndQueryExpr vr =
    let pathPart = pathExpr (vrPath vr)
        queryFields = vrQueryFields vr
     in if null queryFields
            then pathPart
            else TH.InfixE
                (Just pathPart)
                (TH.VarE '(<>))
                (Just (TH.AppE (TH.VarE 'renderQueryString) (queryPairsExp queryFields)))

-- | Build a list expression @[(\"urlName1\", Just \"value1\"), …]@ from
-- the query-field list. The URL key is @urlName@ (from the DSL's
-- @?urlName@ suffix), the value comes from the record field @fieldName@.
-- 'Maybe' fields only emit @Just@ when the value is @Just@; list fields
-- emit one tuple per element; required fields always emit @Just@.
queryPairsExp :: [(Text, Text, QueryFieldKind, Type)] -> Exp
queryPairsExp fields =
    let byTupleItem (urlName, fieldName, kind, _) =
            let fieldVarE = TH.VarE (TH.mkName (Text.unpack fieldName))
                nameBsE = TH.SigE
                    (TH.LitE (TH.StringL (Text.unpack urlName)))
                    (TH.ConT (TH.mkName "ByteString"))
             in case kind of
                    QFRequired ->
                        -- [(urlName, Just (renderCapture field))]
                        TH.ListE
                            [ TH.TupE
                                [ Just nameBsE
                                , Just (TH.AppE (TH.ConE 'Just)
                                        (TH.AppE (TH.VarE renderCaptureFn) fieldVarE))
                                ]
                            ]
                    QFOptional ->
                        -- [(urlName, fmap renderCapture field)]   — Nothing omitted
                        TH.ListE
                            [ TH.TupE
                                [ Just nameBsE
                                , Just (TH.AppE
                                        (TH.AppE (TH.VarE 'fmap) (TH.VarE renderCaptureFn))
                                        fieldVarE)
                                ]
                            ]
                    QFList ->
                        -- map (\v -> (urlName, Just (renderCapture v))) field
                        TH.AppE
                            (TH.AppE (TH.VarE 'map)
                                (TH.LamE [TH.VarP (TH.mkName "_qvElem")]
                                    (TH.TupE
                                        [ Just nameBsE
                                        , Just (TH.AppE (TH.ConE 'Just)
                                                (TH.AppE (TH.VarE renderCaptureFn)
                                                    (TH.VarE (TH.mkName "_qvElem"))))
                                        ])))
                            fieldVarE
        pieces = map byTupleItem fields
     in foldr1 (\a b -> TH.InfixE (Just a) (TH.VarE '(<>)) (Just b)) pieces

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
-- Code generation — trie value
---------------------------------------------------------------------------

-- | Top-level binding name for the per-controller trie value: e.g.
-- @PostsController@ becomes @postsControllerTrie@. Both the emitter
-- ('emitTrieValue') and any IHP-side wrapper that wants to call the
-- binding with a specific dispatch function (e.g.
-- @IHP.Router.IHP.emitCanRoute@) use this helper to agree on the name.
trieValueName :: Name -> Name
trieValueName tyName =
    let base = TH.nameBase tyName
        firstLower [] = []
        firstLower (c : cs) = Char.toLower c : cs
     in TH.mkName (firstLower base <> "Trie")

-- | Build the @buildRouteTrie [(methods, pattern, handler), …]@ expression
-- for a list of validated routes. The @dispatchFnName@ is threaded into
-- each handler via 'handlerExpr' — generic callers (the future
-- @ihp-router@ public quoter) emit it as a fresh lambda parameter, while
-- the IHP-flavoured emitter passes 'runActionPrimeFn' directly.
emitTrieFragment :: Name -> [ValidatedRoute] -> Q Exp
emitTrieFragment dispatchFnName vs = do
    entries <- traverse (emitOneEntry dispatchFnName) vs
    let listE = TH.ListE entries
    pure (TH.AppE (TH.VarE 'buildRouteTrie) listE)

-- | HTTP semantics: if a route accepts GET it should also accept HEAD
-- (HEAD returns the same headers with no body). The existing 'get'
-- helper in IHP.RouterSupport registers both methods automatically;
-- we preserve that behaviour here so @HEAD /foo@ doesn't 405 when the
-- DSL declares @GET /foo@.
expandHeadForGet :: [Method] -> [Method]
expandHeadForGet ms
    | GET `elem` ms && not (HEAD `elem` ms) = ms <> [HEAD]
    | otherwise = ms

-- | Build one trie entry per route — a tuple
-- @([StdMethod], [PatternSegment], WaiHandler)@ where the methods list
-- carries every method that should resolve to this handler.
--
-- Pre-extraction the splice emitted one tuple per (method, route) pair;
-- expanding @GET@ to @[GET, HEAD]@ doubled the number of handler
-- lambdas in the user's @Routes.hs@ Core. With the multi-method shape
-- only one handler closure is built per route — the trie's
-- 'insertRouteMethods' registers it under each listed method.
emitOneEntry :: Name -> ValidatedRoute -> Q Exp
emitOneEntry dispatchFnName vr = do
    handlerE <- handlerExpr dispatchFnName vr
    let methodsList = TH.ListE
            [ TH.ConE (stdMethodCon m)
            | m <- expandHeadForGet (vrMethods vr)
            ]
    pure $ TH.TupE
        [ Just methodsList
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

    -- 'captureSpec' no longer takes a type argument: the trie doesn't
    -- validate capture types at walk time, so only the name is needed.
    -- The handler decodes via 'parseCapture @FieldType' once per request.
    captureSpecE :: Text -> Type -> Exp
    captureSpecE n _ty =
        TH.AppE (TH.VarE 'captureSpec)
            (TH.LitE (TH.StringL (Text.unpack n)))

-- | Build the @Captures -> Application@ handler for one route, as a
-- call to either 'mkHandler' (no query params) or 'mkHandlerQ' (any
-- query params declared via @?name@).
--
-- The runtime helpers do the WAI plumbing ('dispatch', pulling
-- @queryString@ off the request, applying the runner). The TH splice
-- emits only the per-route capture-decoding closure:
--
-- @
--   mkHandler  dispatchFn (\\captures        -> case captures of [bs0, bs1] -> do …pure (Just …); _ -> Nothing)
--   mkHandlerQ dispatchFn (\\captures query  -> case captures of [bs0]      -> do …pure (Just …); _ -> Nothing)
-- @
--
-- The first argument @dispatchFnName@ is the 'Name' of the dispatch
-- function passed to 'mkHandler' \/ 'mkHandlerQ'. Generic callers emit
-- the fresh lambda parameter from 'emitTrieValue' (so plain WAI users
-- can plug in their own dispatch); the IHP-flavoured 'emitCanRoute'
-- passes 'runActionPrimeFn' (a 'mkName' reference resolved at splice
-- use-site to IHP's @runAction'@).
--
-- This shrinks the per-route Core compared to inlining the WAI shell:
-- no per-route 3-arg lambda, no per-route @let _dslQuery = queryString req@,
-- no per-route @dispatch dispatchFn … req respond@ chain, and the
-- arity-mismatch arm is @Nothing@ instead of a 65-character @error@
-- string literal.
--
-- Each capture is still parsed exactly once per request, via
-- 'parseCapture' specialised to the field's Haskell type by
-- @AppTypeE@. No 'Dynamic' round-trip.
handlerExpr :: Name -> ValidatedRoute -> Q Exp
handlerExpr dispatchFnName vr = do
    let captureSegs = [ (n, ty) | VSCapture n ty <- vrPath vr ]
                   <> [ (n, ty) | VSSplat n ty <- vrPath vr ]
        queryFields = vrQueryFields vr
        -- Raw-bytestring pattern names for path captures, in path order.
        bsNames = [ TH.mkName ("_bs" <> show (i :: Int))
                  | i <- [0 .. length captureSegs - 1]
                  ]
        -- Decoded path-capture value names, same order.
        pathValueNames = [ TH.mkName ("_pv" <> show (i :: Int))
                         | i <- [0 .. length captureSegs - 1]
                         ]
        -- Decoded query-field value names.
        queryValueNames = [ TH.mkName ("_qv" <> show (i :: Int))
                          | i <- [0 .. length queryFields - 1]
                          ]
        capturesName = TH.mkName "_dslCaptures"
        queryName    = TH.mkName "_dslQuery"

    let listPat :: Pat
        listPat = TH.ListP (map TH.VarP bsNames)

        -- Each path capture: _pvN <- parseCapture @FieldType _bsN
        pathBindStmts =
            [ TH.BindS
                (TH.VarP vn)
                (TH.AppE
                    (TH.AppTypeE (TH.VarE parseCaptureFn) ty)
                    (TH.VarE bn))
            | ((_, ty), bn, vn) <- zip3 captureSegs bsNames pathValueNames
            ]

        -- Each query field: _qvN <- queryParam<Kind> @Inner "urlName" _dslQuery
        -- The lookup key in the request's query string is the DSL's urlName
        -- (what the user wrote in @?urlName@), not the record fieldName —
        -- these differ when the user declared a { field = #urlName } rebinding.
        -- For QFOptional / QFList the outer wrapper is @Just@ so the
        -- Maybe-do never fails on absent values (they decode to Nothing /
        -- [], respectively).
        queryBindStmts =
            [ TH.BindS
                (TH.VarP vn)
                (queryExtractExp kind innerTy urlName)
            | ((urlName, _, kind, innerTy), vn) <- zip queryFields queryValueNames
            ]

        -- Map capture name → value name so record construction can
        -- wire the right bytestring into the right field.
        pathFieldBindings :: [(Name, Exp)]
        pathFieldBindings =
            [ (fieldNameFor vr capName, TH.VarE vn)
            | ((capName, _), vn) <- zip captureSegs pathValueNames
            ]

        -- Query-param binding: record-field name → decoded value.
        queryFieldBindings :: [(Name, Exp)]
        queryFieldBindings =
            [ (TH.mkName (Text.unpack fieldName), TH.VarE vn)
            | ((_, fieldName, _, _), vn) <- zip queryFields queryValueNames
            ]

        allFieldBindings = pathFieldBindings <> queryFieldBindings

        recordExp = case allFieldBindings of
            [] -> TH.ConE (coName (vrCon vr))
            _  -> TH.RecConE (coName (vrCon vr)) allFieldBindings

        returnStmt = TH.NoBindS (TH.AppE (TH.ConE 'Just) recordExp)

        allBinds = pathBindStmts <> queryBindStmts

        -- The Maybe-builder body: parse captures + query params, then
        -- construct the action. With zero binds, simplifies to a bare
        -- @Just Action@ (no do-block), which produces noticeably less
        -- Core for the common static-route case.
        maybeExp = case allBinds of
            [] -> TH.AppE (TH.ConE 'Just) recordExp
            _  -> TH.DoE Nothing (allBinds <> [returnStmt])

        -- Per-route closure body. The case has only one productive arm
        -- (the right-arity match); the trie's 'insertRouteMethods'
        -- guarantees we'll always be called with the right arity. The
        -- @_ -> Nothing@ fallback exists only to keep the pattern
        -- exhaustive — it should never fire at runtime.
        captureCase =
            TH.CaseE (TH.VarE capturesName)
                [ TH.Match listPat (TH.NormalB maybeExp) []
                , TH.Match TH.WildP
                    (TH.NormalB (TH.ConE 'Nothing))
                    []
                ]

        -- If any captures appear, we need the case to bind them. With
        -- zero captures, the case is a single arm against @[]@ — we
        -- can skip the case entirely and yield 'maybeExp' directly.
        decoderBody
            | null captureSegs = maybeExp
            | otherwise        = captureCase

        -- Pick the helper based on whether any query params are in play.
        -- 'mkHandlerQ' threads the query string; 'mkHandler' doesn't —
        -- so static and path-only routes don't pay for @queryString req@.
        hasQuery = not (null queryFields)

        -- When there are no path captures, the captures-arg is unused
        -- in the body — bind a wildcard to avoid -Wunused-matches.
        capturesPat
            | null captureSegs = TH.WildP
            | otherwise        = TH.VarP capturesName

        handlerBody
            | hasQuery =
                -- mkHandlerQ <dispatchFn> (\captures query -> decoderBody)
                TH.AppE
                    (TH.AppE (TH.VarE 'mkHandlerQ) (TH.VarE dispatchFnName))
                    (TH.LamE [capturesPat, TH.VarP queryName] decoderBody)
            | otherwise =
                -- mkHandler <dispatchFn> (\captures -> decoderBody)
                TH.AppE
                    (TH.AppE (TH.VarE 'mkHandler) (TH.VarE dispatchFnName))
                    (TH.LamE [capturesPat] decoderBody)

    pure handlerBody
  where
    -- Expression that extracts a single query field from _dslQuery.
    -- The lookup key is the DSL's @urlName@ (the name after @?@),
    -- which may differ from the record field name when the user
    -- wrote a @{ field = #urlName }@ rebinding. Shape varies by
    -- kind so the outer Maybe-do always succeeds for optional / list
    -- fields.
    queryExtractExp :: QueryFieldKind -> Type -> Text -> Exp
    queryExtractExp kind innerTy urlName =
        let queryName = TH.mkName "_dslQuery"
            nameLitE = TH.LitE
                (TH.StringL (Text.unpack urlName))
            -- Use stringE to get a ByteString via OverloadedStrings;
            -- the runtime helpers want a ByteString key.
            nameBsE = TH.SigE nameLitE
                (TH.ConT (TH.mkName "ByteString"))
         in case kind of
                QFRequired ->
                    TH.AppE
                        (TH.AppE
                            (TH.AppTypeE (TH.VarE 'queryParamRequired) innerTy)
                            nameBsE)
                        (TH.VarE queryName)
                QFOptional ->
                    -- queryParamOptional returns Maybe a; we want the whole
                    -- thing wrapped so the do block doesn't fail on absence.
                    TH.AppE (TH.ConE 'Just)
                        (TH.AppE
                            (TH.AppE
                                (TH.AppTypeE (TH.VarE 'queryParamOptional) innerTy)
                                nameBsE)
                            (TH.VarE queryName))
                QFList ->
                    TH.AppE (TH.ConE 'Just)
                        (TH.AppE
                            (TH.AppE
                                (TH.AppTypeE (TH.VarE 'queryParamList) innerTy)
                                nameBsE)
                            (TH.VarE queryName))

-- | Unqualified reference to 'IHP.Router.Capture.parseCapture'. Resolved
-- at splice use-site, so the caller just needs 'parseCapture' in scope
-- (provided by @IHP.RouterPrelude@ via @IHP.Router.Capture@).
parseCaptureFn :: Name
parseCaptureFn = TH.mkName "parseCapture"

