{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module IHP.ViewErrorMessages where

--instance TypeError (Text "Cannot 'Show' functions." :$$: Text "Perhaps there is a missing argument?") => Show (a -> b) where
--   showsPrec = error "unreachable"