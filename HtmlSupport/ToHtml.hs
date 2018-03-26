{-# LANGUAGE  UndecidableInstances, OverlappingInstances #-}

module Foundation.HtmlSupport.ToHtml where
import qualified Text.Blaze.Html5              as Html5
import qualified Text.Blaze.Internal
import ClassyPrelude (Text, Show, show)
import Data.String.Conversions (cs)
import Foundation.View.ConvertibleStrings ()
import qualified Data.ByteString

class ToHtml a where
    toHtml :: a -> Html5.Html

instance ToHtml (Text.Blaze.Internal.MarkupM ()) where
    toHtml a = a

instance ToHtml Text where
    toHtml = cs

instance ToHtml Data.ByteString.ByteString where
    toHtml value = toHtml (cs value :: Text)

instance Show a => ToHtml a where
    toHtml value = cs (show value)