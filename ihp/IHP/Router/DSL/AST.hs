{-|
Module: IHP.Router.DSL.AST
Description: AST for the explicit-routes DSL

Pure data types describing a parsed @[routes| ... |]@ block. No Template
Haskell — this module is safe to import from both the parser (compile-time
textual layer) and the TH splice (compile-time code-generation layer).

The grammar, informally (RFC 6570 URI-template syntax for path parameters):

> [routes|ControllerName
> GET    /posts                 PostsAction
> POST   /posts                 CreatePostAction
> GET    /posts/{postId}        ShowPostAction
> GET    /posts/{postId}/edit   EditPostAction
> PATCH  /posts/{postId}        UpdatePostAction
> DELETE /posts/{postId}        DeletePostAction
> |]

@{name}@ binds the segment to a record field of the same name on the
action constructor. @{name:Type}@ is an explicit-type escape hatch.
@{+name}@ (RFC 6570 reserved-string expansion) matches the rest of the path.
@GET|POST@ allows multiple methods for one route. Anything after @--@ on a
line is a comment.
-}
module IHP.Router.DSL.AST
    ( Routes (..)
    , Route (..)
    , PathSeg (..)
    , ActionRef (..)
    , Method (..)
    , methodFromText
    , methodToText
    , expandAnyMethod
    ) where

import Prelude
import Data.Text (Text)

-- | A complete parsed @[routes| ... |]@ block.
--
-- The three header forms the DSL accepts:
--
-- * __Single-controller__ — bare identifier:
--   @\'controllerName\' = Just \"PostsController\"@, @\'appType\' = Nothing@
-- * __Multi-controller, no app binding__ — empty header:
--   both fields 'Nothing'
-- * __Multi-controller with app binding__ — @for \<AppType\>@:
--   @\'appType\' = Just \"WebApplication\"@, @\'controllerName\' = Nothing@.
--   In this form the TH splice emits an additional top-level value
--   @webRoutes :: [ControllerRoute WebApplication]@ (name derived by
--   lower-casing the first letter and stripping any @\"Application\"@
--   suffix) so the user can splat it into 'FrontController.controllers'
--   instead of listing each 'parseRoute' by hand.
data Routes = Routes
    { controllerName :: !(Maybe Text)
    , appType        :: !(Maybe Text)
    , routes         :: ![Route]
    }
    deriving (Eq, Show)

-- | A single route: one or more methods, a path pattern, and an action
-- reference.
data Route = Route
    { routeMethods :: ![Method]
    , routePath    :: ![PathSeg]
    , routeAction  :: !ActionRef
    , routeLine    :: !Int      -- source line number (1-based) for error messages
    }
    deriving (Eq, Show)

-- | A single segment of a route path.
data PathSeg
    = Literal !Text
        -- ^ A literal path piece like @"posts"@ in @/posts/new@.
    | Capture !Text !(Maybe Text)
        -- ^ @{name}@ or @{name:Type}@ — captures one segment, bound to the
        -- action field of the same name. The optional 'Text' is a raw
        -- Haskell type expression (escape hatch for when the capture type
        -- can't be inferred from the record field alone).
    | Splat !Text !(Maybe Text)
        -- ^ @{+name}@ or @{+name:Type}@ — RFC 6570 reserved-string form:
        -- captures the remainder of the path including any @\/@ characters.
        -- Default type is 'Data.Text.Text'.
    deriving (Eq, Show)

-- | Reference to an action constructor, optionally with explicit
-- field-to-capture bindings for when a field name in the constructor
-- differs from the capture name in the path.
data ActionRef = ActionRef
    { actionName    :: !Text
    , fieldBindings :: ![(Text, Text)]
        -- ^ Pairs of @(fieldName, captureName)@. Empty when the record
        -- fields match the capture names directly (the common case).
    }
    deriving (Eq, Show)

-- | HTTP methods recognized by the DSL.
data Method
    = GET
    | POST
    | PUT
    | PATCH
    | DELETE
    | HEAD
    | OPTIONS
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Parse a method name into a 'Method'. @ANY@ is handled by
-- 'expandAnyMethod' rather than here, because it expands to all methods
-- rather than picking one.
methodFromText :: Text -> Maybe Method
methodFromText t = case t of
    "GET"     -> Just GET
    "POST"    -> Just POST
    "PUT"     -> Just PUT
    "PATCH"   -> Just PATCH
    "DELETE"  -> Just DELETE
    "HEAD"    -> Just HEAD
    "OPTIONS" -> Just OPTIONS
    _         -> Nothing

-- | Render a 'Method' as its canonical uppercase name.
methodToText :: Method -> Text
methodToText m = case m of
    GET     -> "GET"
    POST    -> "POST"
    PUT     -> "PUT"
    PATCH   -> "PATCH"
    DELETE  -> "DELETE"
    HEAD    -> "HEAD"
    OPTIONS -> "OPTIONS"

-- | @ANY@ expands to every supported method. Used by the parser when the
-- method field on a route line is literally @ANY@.
expandAnyMethod :: [Method]
expandAnyMethod = [minBound .. maxBound]
