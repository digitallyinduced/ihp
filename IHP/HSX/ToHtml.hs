{-# LANGUAGE  UndecidableInstances #-}

module IHP.HSX.ToHtml where

import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Internal
import ClassyPrelude
import Data.String.Conversions (cs)
import IHP.View.ConvertibleStrings ()
import qualified Data.ByteString

class ToHtml a where
    toHtml :: a -> Html5.Html

instance ToHtml (Text.Blaze.Internal.MarkupM ()) where
    {-# INLINE toHtml #-}
    toHtml a = a

instance ToHtml Text where
    {-# INLINE toHtml #-}
    toHtml = cs

instance ToHtml String where
    {-# INLINE toHtml #-}
    toHtml = cs

instance ToHtml Data.ByteString.ByteString where
    {-# INLINE toHtml #-}
    toHtml value = toHtml (cs value :: Text)

instance {-# OVERLAPPABLE #-} ToHtml a => ToHtml (Maybe a) where
    {-# INLINE toHtml #-}
    toHtml maybeValue = maybe mempty toHtml maybeValue

instance {-# OVERLAPPABLE #-} Show a => ToHtml a where
    {-# INLINE toHtml #-}
    toHtml value = cs (show value)
