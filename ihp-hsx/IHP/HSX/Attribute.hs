{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.HSX.Attribute
Copyright: (c) digitally induced GmbH, 2023
-}
module IHP.HSX.Attribute
( AttributeConverter (..)
) where

import Prelude
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as Html5
import Text.Blaze.Internal (attribute, MarkupM (Parent, Leaf), StaticString (..))
import Data.String.Conversions
import IHP.HSX.ToHtml
import qualified Data.Text as Text
import Data.Text (Text)
import IHP.HSX.Html
import Data.ByteString

class AttributeConverter value where
    attributeValueToText :: Text -> value -> Maybe Html

instance AttributeConverter Bool where
    attributeValueToText name True =
        Just if "data-" `Text.isPrefixOf` name
            then preEscapedToHtml "\"true\"" -- "true" for data attributes
            else "\"" <> textToHtml name <> "\"" -- normal html boolean attriubtes, like <input disabled="disabled"/>, see https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    attributeValueToText name False | "data-" `Text.isPrefixOf` name = Just $ preEscapedToHtml "\"false\"" -- data attribute set to "false"
    attributeValueToText name value = Nothing -- html boolean attribute, like <input disabled/> will be dropped as there is no other way to specify that it's set to false
    {-# INLINE attributeValueToText #-}

instance AttributeConverter attribute => AttributeConverter (Maybe attribute) where
    attributeValueToText name (Just value) = attributeValueToText name value
    attributeValueToText name Nothing = Nothing
    {-# INLINE attributeValueToText #-}

instance AttributeConverter Text where
    attributeValueToText name value = Just $ preEscapedToHtml name <> preEscapedToHtml "=\"" <> textToHtml value <> preEscapedToHtml "\""

--instance AttributeConverter Html5.AttributeValue where
--    attributeValueToText name value = mempty

instance AttributeConverter Html where
    attributeValueToText name value = Just $ "\"" <> value <> "\""

instance AttributeConverter ByteString where
    attributeValueToText name value = attributeValueToText name (cs @ByteString @Text value)

    -- applyAttribute attr attr' value h = h ! (attribute (Html5.textTag attr) (Html5.textTag attr') value)

--instance {-# OVERLAPPABLE #-} ConvertibleStrings value Html5.AttributeValue => ApplyAttribute value where
--    applyAttribute attr attr' value h = applyAttribute attr attr' ((cs value) :: Html5.AttributeValue) h
--    {-# INLINE applyAttribute #-}