{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: IHP.Router.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Router.Types where

import Prelude
import Data.ByteString (ByteString)
import Control.Exception (Exception)
import Network.HTTP.Types.Method
import qualified Data.HashMap.Strict as HashMap
import Data.Attoparsec.ByteString.Char8 (Parser)
import Network.Wai (Application)
import IHP.Router.Trie (RouteTrie)

-- | A controller route entry. Three flavours:
--
-- * 'ControllerRouteMap' — the legacy 'AutoRoute' path: a pre-built HashMap
--   of full paths to handlers, plus a custom-routes fallback 'Parser'.
-- * 'ControllerRouteParser' — standalone custom 'Parser' (for helpers like
--   'IHP.RouterSupport.get', 'IHP.RouterSupport.post', 'IHP.RouterSupport.webSocketApp',
--   'IHP.RouterSupport.startPage', etc.).
-- * 'ControllerRouteTrie' — the new explicit-routes path: a pre-built trie
--   fragment carrying method-aware, capture-aware route entries. Used by
--   the @routes@ quasi-quoter.
--
-- 'IHP.RouterSupport.frontControllerToWAIApp' first merges all 'ControllerRouteTrie'
-- fragments into a single 'RouteTrie' and tries a fast, method-aware lookup against
-- it. On no match it falls back to the legacy 'ControllerRouteMap' HashMap scan
-- and, finally, to Attoparsec for the remaining custom parsers.
data ControllerRoute application
    = ControllerRouteMap
        !(HashMap.HashMap ByteString (application -> Application))
        (Parser Application)
        -- ^ Auto-route HashMap + custom routes fallback parser (lazy — only evaluated on HashMap miss)
    | ControllerRouteParser !(Parser Application)
        -- ^ Custom route parser (for get, post, webSocketApp, startPage, etc.)
    | ControllerRouteTrie !RouteTrie
        -- ^ Pre-built trie fragment from the @routes@ DSL. Method-aware; merged
        -- into the app-wide trie at startup.

data TypedAutoRouteError
    = BadType
        { expectedType :: !ByteString
        , value :: !(Maybe ByteString)
        , field :: !ByteString
        }
    | TooFewArguments
    | NotMatched
    -- | Thrown when 'IHP.RouterSupport.parseUUIDArgument', 'IHP.RouterSupport.parseIntArgument', etc. get passed an invalid value
    --
    -- Let's say we have a @ShowProjectAction { id :: Id Project }@.
    --
    -- When opening @/ShowProject?projectId=ab55d579-80cd-4608-9a8f-c76dea6c2332@ everything is fine.
    -- But when opening @/ShowProject?projectId=not-an-uuid@ this exception will be thrown.
    | NoConstructorMatched
        { expectedType :: !ByteString
        , value :: !(Maybe ByteString)
        , field :: !ByteString
        }
    deriving (Show, Exception)

-- | Thrown e.g. a @CreateProjectAction@ is called from a GET request
--
data UnexpectedMethodException
    = UnexpectedMethodException
    { allowedMethods :: [StdMethod]
    , method :: StdMethod
    }
    deriving (Show, Exception)