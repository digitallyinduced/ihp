{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TurboHaskell.ViewErrorMessages where

import ClassyPrelude
import GHC.TypeLits

--instance TypeError (Text "Cannot 'Show' functions." :$$: Text "Perhaps there is a missing argument?") => Show (a -> b) where
--   showsPrec = error "unreachable"