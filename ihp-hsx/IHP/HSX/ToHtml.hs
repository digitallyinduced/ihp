{-# LANGUAGE  UndecidableInstances #-}
{-# LANGUAGE  FlexibleInstances #-}

{-|
Module: IHP.HSX.ToHtml
Description: Provides a few helper instances that convert data structures to HTML
Copyright: (c) digitally induced GmbH, 2022
-}
module IHP.HSX.ToHtml where

import Prelude
import qualified Text.Blaze.Html5 as Html5
import qualified Text.Blaze.Internal
import Data.Text
import Data.ByteString
import Data.String.Conversions (cs)
import IHP.HSX.ConvertibleStrings ()
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Data.Text as Text
import IHP.HSX.Html

class ToHtml a where
    toHtml :: a -> Html

instance ToHtml (Text.Blaze.Internal.MarkupM ()) where
    {-# INLINE toHtml #-}
    toHtml html = textToHtml (cs (Blaze.renderHtml html))

instance ToHtml Text where
    {-# INLINE toHtml #-}
    toHtml text = textToHtml text

instance ToHtml String where
    {-# INLINE toHtml #-}
    toHtml string = textToHtml (cs string)

instance ToHtml ByteString where
    {-# INLINE toHtml #-}
    toHtml value = textToHtml (cs value)

instance {-# OVERLAPPABLE #-} ToHtml a => ToHtml (Maybe a) where
    {-# INLINE toHtml #-}
    toHtml maybeValue = maybe mempty toHtml maybeValue

instance {-# OVERLAPPABLE #-} Show a => ToHtml a where
    {-# INLINE toHtml #-}
    toHtml value = toHtml (show value)

instance ToHtml Html where
    toHtml value = value