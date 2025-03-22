{-|
Module: IHP.HSX.Lucid2.Attribute
Copyright: (c) digitally induced GmbH, 2023
-}
module IHP.HSX.Lucid2.Attribute
  ( LucidAttributeRaw (..)
  , LucidAttributeValue (..)
  ) where

import Prelude
import qualified Data.Text as Text
import Data.Text (Text)
import Lucid.Base (Attributes, makeAttributes, makeAttributesRaw)

class LucidAttributeValue value where
    buildAttribute :: Text -> value -> Attributes

instance LucidAttributeValue Bool where
    buildAttribute attr True = makeAttributes attr value
        where
            value = if "data-" `Text.isPrefixOf` attr
                    then "true" -- "true" for data attributes
                    else attr -- normal html boolean attriubtes, like <input disabled="disabled"/>, see https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes
    buildAttribute attr False | "data-" `Text.isPrefixOf` attr = makeAttributesRaw attr "false" -- data attribute set to "false"
    buildAttribute attr False = mempty -- html boolean attribute, like <input disabled/> will be dropped as there is no other way to specify that it's set to false
    {-# INLINE buildAttribute #-}

instance LucidAttributeValue lav => LucidAttributeValue (Maybe lav) where
    buildAttribute attr (Just value) = buildAttribute attr value
    buildAttribute attr Nothing = mempty
    {-# INLINE buildAttribute #-}

instance LucidAttributeValue Text where
    buildAttribute = makeAttributes
    {-# INLINE buildAttribute #-}

newtype LucidAttributeRaw = MkLucidAttributeRaw { unLucidAttributeRaw :: Text }

instance LucidAttributeValue LucidAttributeRaw where
    buildAttribute attr MkLucidAttributeRaw { unLucidAttributeRaw = textVal } =
      makeAttributesRaw attr textVal
    {-# INLINE buildAttribute #-}
