module IHP.HSX.Html
( preEscapedToHtml
, renderHtml
, Html
, spaceSep
, concat
, textToHtml
, spaceSepWithLeadingSpace
, renderHtmlBuilder
, concat
, renderMarkup
) where

import Prelude hiding (concat)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Coerce (coerce)
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax as TH
import Data.String
import qualified Data.Binary.Builder as Builder
import qualified Data.Text.Encoding as Text

-- We have to keep the type variable to allow for a Monad instance
-- The Monad instance is needed to keep b.c. with existing IHP apps
newtype Html' a = Html Text
type Html = Html' ()

instance Monoid Html where
    mempty = Html ""

instance Semigroup Html where
    Html first <> Html second = Html (first <> second)

preEscapedToHtml :: Text -> Html
preEscapedToHtml preEscaped = Html preEscaped

renderHtml :: Html -> Text
renderHtml (Html html) = html

renderMarkup :: Html -> Text
renderMarkup = renderHtml

instance TH.Lift Html where
    lift (Html text) = [| Html $(TH.lift text) |]
    liftTyped (Html text) = [|| Html $$(TH.liftTyped text) ||]

spaceSep :: [Html] -> Html
spaceSep values = Html (Text.unwords (coerce values))

spaceSepWithLeadingSpace :: [Html] -> Html
spaceSepWithLeadingSpace values =
    let
        spaceSep'@(Html spaceSeperatedHtml) = spaceSep values
    in
        if Text.null spaceSeperatedHtml
            then spaceSep'
            else " " <> spaceSep'

concat :: [Html] -> Html
concat values = Html (Text.concat (coerce values))

escapeChar :: Char -> Text
escapeChar char =
    case char of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        x -> Text.singleton x

textToHtml :: Text -> Html
textToHtml text =
    Html (Text.concatMap escapeChar text)

instance IsString Html where
    fromString string = preEscapedToHtml (Text.pack string)


instance Functor Html' where
    fmap f (Html text) = undefined

instance Applicative Html' where
    pure value = (Html "")
    Html a <*> Html b = Html (a <> b)

instance Monad Html' where
    return = pure
    (>>=) :: forall a b. Html' a -> (a -> Html' b) -> Html' b
    Html x >>= f = Html x

renderHtmlBuilder :: Html -> Builder.Builder
renderHtmlBuilder html = Text.encodeUtf8Builder (renderHtml html)