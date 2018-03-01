{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ViewSupport (ViewContext (ViewContext), Html, ToAttributeValue (toAttributeValue)) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import View.Context

type Html = (?viewContext :: ViewContext) => Html5.Html

class ToAttributeValue a where
    toAttributeValue :: a -> Html5.AttributeValue

instance ToAttributeValue Html5.AttributeValue where
    toAttributeValue = id

instance ToAttributeValue String where
    toAttributeValue = Html5.stringValue