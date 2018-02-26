{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Foundation.UrlGeneratorSupport where
import           ClassyPrelude
import           Foundation.ModelSupport
import           GHC.Records

class UrlArgument a where
    toText :: a -> Text

instance HasId a => UrlArgument a where
    toText model = tshow (getId model)

instance UrlArgument Int where
    toText int = tshow int
