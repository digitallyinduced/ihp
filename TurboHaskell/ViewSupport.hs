{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module TurboHaskell.ViewSupport (HtmlWithContext, classes, CreateViewContext (createViewContext, ControllerContext), Layout, View (..)) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import TurboHaskell.HaskellSupport
import TurboHaskell.ControllerSupport  (RequestContext (RequestContext))
import TurboHaskell.ModelSupport
import qualified Data.Aeson as JSON
import qualified Data.Text as Text

type HtmlWithContext context = (?viewContext :: context) => Html5.Html
type Layout = Html5.Html -> Html5.Html

{-# INLINE classes #-}
classes :: [(Text, Bool)] -> Text
classes classNameBoolPairs =
    classNameBoolPairs
    |> filter snd
    |> map fst
    |> unwords

class CreateViewContext viewContext where
    type ControllerContext viewContext :: *
    createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext viewContext, ?modelContext :: ModelContext) => IO viewContext



class View theView viewContext | theView -> viewContext where
    beforeRender :: (?viewContext :: viewContext) => (viewContext, theView) -> (viewContext, theView)
    {-# INLINE beforeRender #-}
    beforeRender view = view
    html :: (?viewContext :: viewContext, ?view :: theView) => theView -> Html5.Html
    json :: theView -> JSON.Value
    json = error "Not implemented"

currentViewId :: (?view :: view, Show view) => Text
currentViewId = fst (Text.breakOn " " (tshow ?view))