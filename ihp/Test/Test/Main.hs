module Main where

import           IHP.Prelude
import           Test.Hspec

import qualified Test.Controller.AccessDeniedSpec
import qualified Test.Controller.ContextSpec
import qualified Test.Controller.CookieSpec
import qualified Test.Controller.NotFoundSpec
import qualified Test.Controller.ParamSpec
import qualified Test.FileStorage.ControllerFunctionsSpec
import qualified Test.HaskellSupportSpec
import qualified Test.ModelSupportSpec
import qualified Test.NameSupportSpec
import qualified Test.PGListenerSpec
import qualified Test.QueryBuilderSpec
import qualified Test.RouterSupportSpec
import qualified Test.ValidationSupport.ValidateFieldSpec
import qualified Test.View.CSSFrameworkSpec
import qualified Test.View.FormSpec
import qualified Test.ViewSupportSpec

main :: IO ()
main = hspec do
    Test.ValidationSupport.ValidateFieldSpec.tests
    Test.NameSupportSpec.tests
    Test.HaskellSupportSpec.tests
    Test.View.CSSFrameworkSpec.tests
    Test.View.FormSpec.tests
    Test.Controller.ContextSpec.tests
    Test.Controller.ParamSpec.tests
    Test.Controller.AccessDeniedSpec.tests
    Test.Controller.NotFoundSpec.tests
    Test.ModelSupportSpec.tests
    Test.QueryBuilderSpec.tests
    Test.RouterSupportSpec.tests
    Test.ViewSupportSpec.tests
    Test.FileStorage.ControllerFunctionsSpec.tests
    Test.Controller.CookieSpec.tests
    Test.PGListenerSpec.tests
    Test.TypedSqlSpec.tests
