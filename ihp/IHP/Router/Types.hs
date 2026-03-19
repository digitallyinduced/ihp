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

-- | A controller route entry — either a pre-built HashMap for O(1) dispatch,
-- or a custom Attoparsec parser for dynamic URL patterns.
--
-- 'ControllerRouteMap' carries both an auto-route HashMap and a fallback parser
-- for custom routes. 'ControllerRouteParser' wraps standalone parsers like 'get', 'post',
-- 'webSocketApp', 'startPage', etc.
--
-- 'frontControllerToWAIApp' scans all 'ControllerRouteMap' HashMaps directly (no Attoparsec)
-- for O(1) dispatch, and only falls back to Attoparsec for custom/dynamic route parsers.
data ControllerRoute application
    = ControllerRouteMap
        !(HashMap.HashMap ByteString (application -> Application))
        !(Parser Application)
        -- ^ Auto-route HashMap + custom routes fallback parser
    | ControllerRouteParser !(Parser Application)
        -- ^ Custom route parser (for get, post, webSocketApp, startPage, etc.)

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