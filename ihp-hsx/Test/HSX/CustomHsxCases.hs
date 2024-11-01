{-|
Module: IHP.HSX.TestHsx
Description: Test helpers for HSX tests with custom tags and attributes
-}
module Test.HSX.CustomHsxCases where

import Test.Hspec
import Prelude
import IHP.HSX.QQ
import qualified Text.Blaze.Renderer.Text as Blaze
import Data.Text
import Language.Haskell.TH.Quote
import IHP.HSX.Parser
import qualified Data.Set as Set

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
