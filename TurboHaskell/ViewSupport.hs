{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module TurboHaskell.ViewSupport (HtmlWithContext, ToAttributeValue (toAttributeValue), classes, CreateViewContext (createViewContext, ControllerContext), Layout, View (..)) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import TurboHaskell.HaskellSupport
import TurboHaskell.ControllerSupport  (RequestContext (RequestContext))
import TurboHaskell.ModelSupport
import qualified Data.Aeson as JSON

type HtmlWithContext context = (?viewContext :: context) => Html5.Html
type Layout = Html5.Html -> Html5.Html

class ToAttributeValue a where
    toAttributeValue :: a -> Html5.AttributeValue

instance ToAttributeValue Html5.AttributeValue where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = id

instance ToAttributeValue String where
    {-# INLINE toAttributeValue #-}
    toAttributeValue = Html5.stringValue

classes :: [(Text, Bool)] -> Text
classes classNameBoolPairs =
    classNameBoolPairs
    |> filter snd
    |> map fst
    |> intercalate " "

class CreateViewContext viewContext where
    type ControllerContext viewContext :: *
    createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext viewContext, ?modelContext :: ModelContext) => IO viewContext



class View theView where
    type ViewContextForView theView :: *
    html :: (?viewContext :: ViewContextForView theView) => theView -> Html5.Html
    json :: theView -> JSON.Value
    json = error "Not implemented"