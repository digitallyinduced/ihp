{-|
Module: Test.View.CSSFrameworkSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.View.CSSFrameworkSpec where

import Test.Hspec
import IHP.Prelude
import IHP.View.Types
import IHP.View.CSSFramework
import IHP.Controller.Session
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H

tests = do
    describe "CSS Framework" do
        describe "bootstrap" do
            let cssFramework = bootstrap

            it "should render a successful flash message" do
                let flashMessage = SuccessFlashMessage "You have successfully registered for an account"
                styledFlashMessage cssFramework cssFramework flashMessage `shouldRenderTo` "<div class=\"alert alert-success\">You have successfully registered for an account</div>"
            
            it "should render a error flash message" do
                let flashMessage = ErrorFlashMessage "You have successfully registered for an account"
                styledFlashMessage cssFramework cssFramework flashMessage `shouldRenderTo` "<div class=\"alert alert-danger\">You have successfully registered for an account</div>"

            describe "submit button" do
                let submitButton = SubmitButton { label = "Save Project" , buttonClass = "my-custom-button" , cssFramework }
                it "should render" do
                    styledSubmitButton cssFramework cssFramework submitButton `shouldRenderTo` "<button class=\"btn btn-primary my-custom-button\">Save Project</button>"
                
                it "should render with empty class" do
                    styledSubmitButton cssFramework cssFramework (submitButton { buttonClass = "" }) `shouldRenderTo` "<button class=\"btn btn-primary\">Save Project</button>"


            describe "text field" do
                let baseTextField = FormField
                        { fieldType = TextInput
                        , fieldName = "firstname"
                        , fieldLabel = "First name:"
                        , fieldValue = ""
                        , fieldInputId = "fname"
                        , validatorResult = Nothing
                        , fieldInput = const H.input
                        , fieldClass = ""
                        , labelClass = ""
                        , disableLabel = False
                        , disableGroup = False
                        , disableValidationResult = False
                        , cssFramework = cssFramework
                        , helpText = ""
                        , placeholder = "Your firstname"
                        , required = False
                        }
                it "should render" do
                    let textField = baseTextField
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"></div>"

                it "should render a validation error" do
                    let textField = baseTextField { validatorResult = Just "should not be empty" }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control is-invalid\"><div class=\"invalid-feedback\">should not be empty</div></div>"

                it "should render with disableLabel = True" do
                    let textField = baseTextField { disableLabel = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"></div>"
                
                it "should render with disableGroup = True" do
                    let textField = baseTextField { disableGroup = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\">"

                it "should render with help text" do
                    let textField = baseTextField { helpText = "Enter your first name" }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"><small class=\"form-text text-muted\">Enter your first name</small></div>"

shouldRenderTo renderFunction expectedHtml = Blaze.renderMarkup renderFunction `shouldBe` expectedHtml
