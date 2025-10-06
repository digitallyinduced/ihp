{-|
Module: Test.HSX.CustomHsxCases
Description: Test helpers for HSX tests with custom tags and attributes
-}
module IHP.HSX.CustomHsxCases where

import Prelude
import qualified IHP.HSX.QQ as Blaze
import qualified IHP.HSX.ToHtml as Blaze
import qualified IHP.HSX.Lucid2.QQ as Lucid2
import qualified IHP.HSX.Lucid2.ToHtml as IHPLucid2
import qualified Text.Blaze as Blaze
import Language.Haskell.TH.Quote
import qualified "template-haskell" Language.Haskell.TH           as TH
import IHP.HSX.Parser
import qualified Data.Set as Set
import qualified "lucid2" Lucid.Base as Lucid2 (Html, HtmlT, ToHtml(..))
import Lucid.Base (generalizeHtmlT)

{- This allows us to share test cases between the blaze and lucid backends for
 - HSX. We build a QuasiQuoter that takes the same string splice as input, and
 - runs it through both backends. It also converts the results into lazy text
 - values for testing purposes.
 -}
customHsx :: HsxSettings -> QuasiQuoter
customHsx settings =
    QuasiQuoter
        { quoteExp = quoteHsxExpressionShared settings
        , quotePat = error "quotePat: not defined"
        , quoteDec = error "quoteDec: not defined"
        , quoteType = error "quoteType: not defined"
        }

hsx :: QuasiQuoter
hsx = customHsx
        (HsxSettings
            { checkMarkup = True
            , additionalTagNames = Set.empty
            , additionalAttributeNames = Set.empty
            }
        )

myCustomHsx :: QuasiQuoter
myCustomHsx = customHsx 
    (HsxSettings { checkMarkup = True
                 , additionalTagNames = Set.fromList ["mycustomtag", "anothercustomtag"]
                 , additionalAttributeNames = Set.fromList ["my-custom-attr", "anothercustomattr"] 
                 }
    )

myTagsOnlyHsx :: QuasiQuoter
myTagsOnlyHsx = customHsx
    (HsxSettings { checkMarkup = True
                 , additionalTagNames = Set.fromList ["mycustomtag", "anothercustomtag"]
                 , additionalAttributeNames = Set.fromList []
                 }
    )

myAttrsOnlyHsx :: QuasiQuoter
myAttrsOnlyHsx = customHsx
    (HsxSettings { checkMarkup = True
                 , additionalTagNames = Set.fromList []
                 , additionalAttributeNames = Set.fromList ["my-custom-attr", "anothercustomattr"]
                 }
    )

data AllBackends = MkAllBackends
  { blazeMarkup :: !Blaze.Markup
  , lucid2Html :: !(Lucid2.Html ())
  , lucid2HtmlM :: !(Lucid2.Html ())
  } deriving Show

instance Lucid2.ToHtml AllBackends where
  toHtml = generalizeHtmlT . lucid2Html
  toHtmlRaw = generalizeHtmlT . lucid2Html

instance (Monad m) => IHPLucid2.EmbedAsHtml (Lucid2.HtmlT m) AllBackends where
    toHtml = IHPLucid2.Lucid2Html . generalizeHtmlT . lucid2HtmlM
    toHtmlRaw = IHPLucid2.Lucid2Html . generalizeHtmlT . lucid2HtmlM

instance Blaze.ToHtml AllBackends where
  toHtml = blazeMarkup

quoteHsxExpressionShared :: HsxSettings -> String -> TH.ExpQ
quoteHsxExpressionShared settings spliceStr =
  let blazeExp = Blaze.quoteHsxExpression settings spliceStr
      lucidExp = Lucid2.quoteHsxExpression settings spliceStr
      lucidExpM = Lucid2.quoteHsxExpressionM settings spliceStr
   in [| MkAllBackends
       { blazeMarkup = $blazeExp
       , lucid2Html = $lucidExp
       , lucid2HtmlM = $lucidExpM
       } |]
