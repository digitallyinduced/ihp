{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Foundation.ViewSupport (HtmlWithContext, ToAttributeValue (toAttributeValue), classes, CreateViewContext (createViewContext)) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import Foundation.HaskellSupport
import Foundation.ControllerSupport  (RequestContext (RequestContext))
import Apps.Web.Controller.Context
import Foundation.ModelSupport

type HtmlWithContext context = (?viewContext :: context) => Html5.Html

class ToAttributeValue a where
    toAttributeValue :: a -> Html5.AttributeValue

instance ToAttributeValue Html5.AttributeValue where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = id

instance ToAttributeValue String where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = Html5.stringValue

classes :: [(Text, Bool)] -> Text
classes classNameBoolPairs = classNameBoolPairs |> filter snd |> map fst |> intercalate " "

class CreateViewContext viewContext where
	createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => IO viewContext