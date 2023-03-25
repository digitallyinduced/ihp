{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: IHP.Router.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Router.Types where

import IHP.Prelude
import Network.HTTP.Types.Method

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