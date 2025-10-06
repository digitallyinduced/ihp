{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.HSX.Attribute
Copyright: (c) digitally induced GmbH, 2023
-}
module IHP.HSX.Attribute
( ApplyAttribute (..)
) where

import Prelude
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html5
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString (..))
import Data.String.Conversions
import IHP.HSX.ToHtml
import qualified Data.Text as Text
import Data.Text (Text)

class ApplyAttribute value where
    applyAttribute :: Text -> Text -> value -> (Html5.Html -> Html5.Html)

instance ApplyAttribute Bool where
    applyAttribute attr attr' True h = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') (Html5.textValue value))
        where
            value = if "data-" `Text.isPrefixOf` attr
                    then "true" -- "true" for data attributes
                    else attr -- normal html boolean attriubtes, like <input disabled="disabled"/>, see https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    applyAttribute attr attr' false h | "data-" `Text.isPrefixOf` attr = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') "false") -- data attribute set to "false"
    applyAttribute attr attr' false h = h -- html boolean attribute, like <input disabled/> will be dropped as there is no other way to specify that it's set to false
    {-# INLINE applyAttribute #-}

instance ApplyAttribute attribute => ApplyAttribute (Maybe attribute) where
    applyAttribute attr attr' (Just value) h = applyAttribute attr attr' value h
    applyAttribute attr attr' Nothing h = h
    {-# INLINE applyAttribute #-}

instance ApplyAttribute Html5.AttributeValue where
    applyAttribute attr attr' value h = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') value)
    {-# INLINE applyAttribute #-}

instance {-# OVERLAPPABLE #-} ConvertibleStrings value Html5.AttributeValue => ApplyAttribute value where
    applyAttribute attr attr' value h = applyAttribute attr attr' ((cs value) :: Html5.AttributeValue) h
    {-# INLINE applyAttribute #-}