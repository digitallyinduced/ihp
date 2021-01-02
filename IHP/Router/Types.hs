{-|
Module: IHP.Router.Types
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Router.Types where

import IHP.Prelude

-- | Thrown when 'IHP.RouterSupport.parseUUIDArgument', 'IHP.RouterSupport.parseIntArgument', etc. get passed an invalid value
--
-- Let's say we have a @ShowProjectAction { id :: Id Project }@.
--
-- When opening @/ShowProject?projectId=ab55d579-80cd-4608-9a8f-c76dea6c2332@ everything is fine.
-- But when opening @/ShowProject?projectId=not-an-uuid@ this exception will be thrown.
data InvalidActionArgumentException = InvalidActionArgumentException
        { expectedType :: ByteString
        , value :: ByteString
        , field :: ByteString
        } deriving (Eq, Show)

instance Exception InvalidActionArgumentException