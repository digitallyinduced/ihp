{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module TurboHaskell.ViewSupport
( HtmlWithContext
, classes
, CreateViewContext (..)
, Layout
, View (..)
, currentViewId
) where

import ClassyPrelude
import qualified Text.Blaze
import qualified Text.Blaze.Html5 as Html5
import TurboHaskell.HaskellSupport
import TurboHaskell.ControllerSupport
import TurboHaskell.ModelSupport
import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Text.Inflections as Inflector
import qualified Data.Either as Either

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
    type ViewApp viewContext
    createViewContext :: (?requestContext :: RequestContext, ?controllerContext :: ControllerContext, ?modelContext :: ModelContext) => IO viewContext



class View theView viewContext | theView -> viewContext where
    beforeRender :: (?viewContext :: viewContext) => (viewContext, theView) -> (viewContext, theView)
    {-# INLINE beforeRender #-}
    beforeRender view = view
    html :: (?viewContext :: viewContext, ?view :: theView) => theView -> Html5.Html
    json :: theView -> JSON.Value
    json = error "Not implemented"

-- Returns a string to be used as a html id attribute for the current view.
-- E.g. when calling `currentViewId` while rendering the view `Web.View.Projects.Show`, this will return `"projects-show"`
--
-- Useful to automatically scope certain css rules to a specific view.
-- Example:
-- 
-- module Web.View.Projects.Show where
-- render = [hsx|<div id={currentViewId}>Hello World!</div>|]
-- 
-- This will render `<div id="projects-show">Hello World!</div>`
currentViewId :: (?view :: view, Typeable view) => Text
currentViewId = 
        case moduleParts of
            [_, "View", controllerName, viewName] -> Either.fromRight (error "currentViewId: Failed to parse controller name") (Inflector.toDashed controllerName) <> "-" <> Either.fromRight (error "currentViewId: Failed to parse view name") (Inflector.toDashed viewName)
            _ -> error ("currentViewId: Failed to read view id for " <> cs moduleName)
    where
        constructor = Typeable.typeRepTyCon (Typeable.typeOf ?view)

        -- Module name: Web.View.Projects.Show
        moduleName :: Text
        moduleName = cs (Typeable.tyConModule constructor)

        moduleParts :: [Text]
        moduleParts = Text.splitOn "." moduleName


        