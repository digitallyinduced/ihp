{-# LANGUAGE FlexibleInstances, DataKinds, PolyKinds, TypeApplications #-}

module Foundation.GeneratedModelSupport where
import ClassyPrelude hiding (id)
import Foundation.ModelSupport
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG
import GHC.Records
import GHC.OverloadedLabels
import Data.String.Conversions (cs)
