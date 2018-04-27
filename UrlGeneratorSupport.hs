{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Foundation.UrlGeneratorSupport where
import           ClassyPrelude
import           Foundation.ModelSupport
import           GHC.Records
import Data.UUID (UUID)
import Model.Generated.Types (HasId, getId, IdType)

class UrlArgument a where
    toText :: a -> Text

instance UrlArgument UUID where
    toText uuid = tshow uuid

instance (HasId a, NewTypeWrappedUUID (IdType a)) => UrlArgument a where
    toText model = tshow $ unwrap (getId model)

class PathTo model where
    pathTo :: model -> Text
