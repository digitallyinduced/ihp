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
    , Method
    , methodFromText
    , methodToText
    , expandAnyMethod
    ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text.Encoding as Text.Encoding
import Network.HTTP.Types.Method (StdMethod (..), parseMethod, renderStdMethod)

-- | A complete parsed @[routes| ... |]@ block.
--
-- * __Single-controller__ — bare identifier header:
--   @\'controllerName\' = Just \"PostsController\"@
-- * __Multi-controller__ — empty header:
--   @\'controllerName\' = Nothing@. The TH splice reifies each action
--   constructor to find its parent type and emits instances per type.
data Routes = Routes
    { controllerName :: !(Maybe Text)
    , routes         :: ![Route]
    }
    deriving (Eq, Show)

-- | A single route: one or more methods, a path pattern, an optional
-- query-param spec, and an action reference.
data Route = Route
    { routeMethods     :: ![Method]
    , routePath        :: ![PathSeg]
    , routeQueryParams :: ![Text]
        -- ^ Query-string parameters declared on the route via the
        -- @?name1&name2@ suffix. Each name is expected to correspond to a
        -- record field of the action constructor (either directly, or via
        -- a @{field = #name}@ rebinding in the 'ActionRef'). Empty list
        -- means no @?@ clause was written — all unbound record fields
        -- must then be the empty set, or the TH splice errors out.
    , routeAction      :: !ActionRef
    , routeLine        :: !Int      -- source line number (1-based) for error messages
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

-- | HTTP methods recognised by the DSL. We reuse 'StdMethod' from
-- @http-types@ rather than defining our own enum — it's the standard
-- type used across WAI and the rest of the Haskell HTTP ecosystem.
type Method = StdMethod

-- | Parse a method name into a 'Method' via @http-types@'s 'parseMethod'.
-- @ANY@ is handled by 'expandAnyMethod' rather than here, because it
-- expands to all methods rather than picking one.
methodFromText :: Text -> Maybe Method
methodFromText t = case parseMethod (Text.Encoding.encodeUtf8 t) of
    Right m -> Just m
    Left _  -> Nothing

-- | Render a 'Method' as its canonical uppercase name.
methodToText :: Method -> Text
methodToText = Text.Encoding.decodeUtf8 . renderStdMethod

-- | @ANY@ expands to every 'StdMethod' constructor.
expandAnyMethod :: [Method]
expandAnyMethod = [minBound .. maxBound]
