{-|
Module: IHP.HSX.TestHsx
Description: Test helpers for HSX tests with custom tags and attributes
-}
module IHP.HSX.CustomHsxCases where

import Test.Hspec
import Prelude
import IHP.HSX.QQ
import qualified Text.Blaze.Renderer.Text as Blaze
import Data.Text
import Language.Haskell.TH.Quote
import IHP.HSX.Parser

myCustomHsx :: QuasiQuoter
myCustomHsx = customHsx 
    (AdditionalTags ["mycustomtag", "anothercustomtag"])
    (AdditionalAttributes ["my-custom-attr", "anothercustomattr"])

myTagsOnlyHsx :: QuasiQuoter
myTagsOnlyHsx = customHsx
    (AdditionalTags ["mycustomtag", "anothercustomtag"])
    (AdditionalAttributes [])

myAttrsOnlyHsx :: QuasiQuoter
myAttrsOnlyHsx = customHsx
    (AdditionalTags [])
    (AdditionalAttributes ["my-custom-attr", "anothercustomattr"])