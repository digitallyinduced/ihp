{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances #-}

module Foundation.UrlGeneratorSupport where
import           ClassyPrelude
import           Foundation.ModelSupport
import           GHC.Records
import Data.UUID (UUID)
import Foundation.HaskellSupport

class UrlArgument a where
    toText :: a -> Text

instance UrlArgument UUID where
    {-# INLINE toText #-}
    toText uuid = tshow uuid

instance forall a fieldType. (HasField "id" a fieldType, NewTypeWrappedUUID fieldType) => UrlArgument a where
    {-# INLINE toText #-}
    toText model = tshow $ unwrap (getField @"id" model)

class PathTo model where
    pathTo :: model -> Text
