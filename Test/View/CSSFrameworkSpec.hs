{-|
Module: Test.View.CSSFrameworkSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.View.CSSFrameworkSpec where

import Test.Hspec
import IHP.Prelude
import IHP.View.Types
import IHP.View.CSSFramework
import IHP.FlashMessages.Types
import IHP.Controller.Session
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H
import IHP.ModelSupport
import IHP.Breadcrumbs.Types
import IHP.Pagination.Types
import qualified IHP.ControllerPrelude as Text

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
                        , disabled = False
                        , disableLabel = False
                        , disableGroup = False
                        , disableValidationResult = False
                        , cssFramework = cssFramework
                        , helpText = ""
                        , placeholder = "Your firstname"
                        , required = False
                        , autofocus = False
                        }
                it "should render" do
                    let textField = baseTextField
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"></div>"

                it "should render with disabled" do
                    let textField = baseTextField { disabled = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\" disabled=\"disabled\"></div>"

                it "should render a validation error" do
                    let textField = baseTextField { validatorResult = Just (TextViolation "should not be empty") }
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


            describe "checkbox" do
                let baseCheckbox = FormField
                        { fieldType = CheckboxInput
                        , fieldName = "is_active"
                        , fieldLabel = "Is Active"
                        , fieldValue =  "yes"
                        , fieldInputId = "user_is_active"
                        , validatorResult = Nothing
                        , fieldClass = ""
                        , labelClass = ""
                        , disabled = False
                        , disableLabel = False
                        , disableGroup = False
                        , disableValidationResult = False
                        , fieldInput = const H.input
                        , cssFramework = cssFramework
                        , helpText = ""
                        , placeholder = ""
                        , required = False
                        , autofocus = False
                        }
                it "should render" do
                    let checkbox = baseCheckbox
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><label class=\"form-check-label\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active</label></div></div>"

                it "should render with disabled" do
                    let checkbox = baseCheckbox { disabled = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><label class=\"form-check-label\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" checked=\"checked\" disabled=\"disabled\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active</label></div></div>"

                it "should render without checked" do
                    let checkbox = baseCheckbox { fieldValue = "no" }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><label class=\"form-check-label\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active</label></div></div>"

                it "should render with disableLabel = True" do
                    let checkbox = baseCheckbox { disableLabel = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><div><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active</div></div></div>"

                it "should render with disableGroup = True" do
                    let checkbox = baseCheckbox { disableGroup = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-check\"><label class=\"form-check-label\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active</label></div>"

                it "should render with help text" do
                    let checkbox = baseCheckbox { helpText = "Is the user active?" }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><label class=\"form-check-label\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\">Is Active<small class=\"form-text text-muted\">Is the user active?</small></label></div></div>"

            describe "select" do
                let baseSelect = FormField
                        { fieldType = SelectInput [("First Value", "a"), ("Second Value", "b")]
                        , fieldName = "user_id"
                        , fieldLabel = "User"
                        , fieldValue = ""
                        , fieldInputId = "project_user_id"
                        , validatorResult = Nothing
                        , fieldClass = ""
                        , labelClass = ""
                        , disabled = False
                        , disableLabel = False
                        , disableGroup = False
                        , disableValidationResult = False
                        , fieldInput = const (H.select mempty)
                        , cssFramework = cssFramework
                        , helpText = ""
                        , placeholder = "Please select"
                        , required = False
                        , autofocus = False
                    }

                it "should render" do
                    let select = baseSelect
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\"><option selected=\"selected\" disabled=\"disabled\">Please select</option><option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select></div>"

                it "should render with disabled" do
                    let select = baseSelect { disabled = True }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\" disabled=\"disabled\"><option selected=\"selected\" disabled=\"disabled\">Please select</option><option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select></div>"

                it "should render with selected" do
                    let select = baseSelect { fieldValue = "b" }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"b\"><option disabled=\"disabled\">Please select</option><option value=\"a\">First Value</option><option value=\"b\" selected=\"selected\">Second Value</option></select></div>"

                it "should render with custom placeholder" do
                    let select = baseSelect { placeholder = "Pick something" }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\"><option selected=\"selected\" disabled=\"disabled\">Pick something</option><option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select></div>"

            describe "pagination" do
                let basePagination = Pagination
                        {
                            pageSize = 3
                        ,   totalItems = 12
                        ,   currentPage = 2
                        ,   window = 3
                        }
                it "should render previous link" do
                    let pagination = basePagination
                    styledPaginationLinkPrevious cssFramework cssFramework pagination "#" `shouldRenderTo` "<li class=\"page-item\"><a class=\"page-link\" href=\"#\" aria-label=\"Previous\"><span aria-hidden=\"true\">&laquo;</span><span class=\"sr-only\">Previous</span></a></li>"

                it "should render previous link disabled on the first page" do
                    let pagination = basePagination { currentPage = 1}
                    styledPaginationLinkPrevious cssFramework cssFramework pagination "#" `shouldRenderTo` "<li class=\"page-item disabled\"><a class=\"page-link\" href=\"#\" aria-label=\"Previous\"><span aria-hidden=\"true\">&laquo;</span><span class=\"sr-only\">Previous</span></a></li>"

                it "should render next link" do
                    let pagination = basePagination
                    styledPaginationLinkNext cssFramework cssFramework pagination "#" `shouldRenderTo` "<li class=\"page-item\"><a class=\"page-link\" href=\"#\" aria-label=\"Next\"><span aria-hidden=\"true\">&raquo;</span><span class=\"sr-only\">Next</span></a></li>"

                it "should render next link disabled on the last page" do
                    let pagination = basePagination { currentPage = 4}
                    styledPaginationLinkNext cssFramework cssFramework pagination "#" `shouldRenderTo` "<li class=\"page-item disabled\"><a class=\"page-link\" href=\"#\" aria-label=\"Next\"><span aria-hidden=\"true\">&raquo;</span><span class=\"sr-only\">Next</span></a></li>"

                it "should render items per page selector" do
                    let pagination = basePagination
                    stylePaginationItemsPerPageSelector cssFramework cssFramework pagination (\n -> cs $ "https://example.com?maxItems=" <> (show n)) `shouldRenderTo` "<option value=\"10\" data-url=\"https://example.com?maxItems=10\">10 items per page</option><option value=\"20\" data-url=\"https://example.com?maxItems=20\">20 items per page</option><option value=\"50\" data-url=\"https://example.com?maxItems=50\">50 items per page</option><option value=\"100\" data-url=\"https://example.com?maxItems=100\">100 items per page</option><option value=\"200\" data-url=\"https://example.com?maxItems=200\">200 items per page</option>"

                it "should render the wrapping pagination" do
                    let pagination = basePagination
                    let paginationView = PaginationView
                            { cssFramework = cssFramework
                            , pagination = pagination
                            , pageUrl = const ""
                            , linkPrevious = mempty
                            , linkNext = mempty
                            , pageDotDotItems = mempty
                            , itemsPerPageSelector = mempty
                            }

                    let render = Blaze.renderMarkup $ styledPagination cssFramework cssFramework paginationView
                    Text.isInfixOf "<div class=\"d-flex justify-content-md-center\">" (cs render) `shouldBe` True

            describe "breadcrumbs" do
                let baseBreadcrumbsItem = BreadcrumbsItem
                        { label = [hsx|First item|]
                        , url = Nothing
                        , isActive = False
                        }
                let baseBreadcrumbs = [breadcrumbsItem]

                it "should render a breadcrumbs item with no link" do
                    let breadcrumbsItem = baseBreadcrumbsItem
                    let breadcrumbs = baseBreadcrumbs

                    styleBreadcrumbsItem cssFramework cssFramework breadcrumbs breadcrumbsItem `shouldBe` ""

                it "should render a breadcrumbs item with link" do
                    let breadcrumbsItem = baseBreadcrumbsItem {url = "https://example.com"}
                    let breadcrumbs = baseBreadcrumbs

                    styleBreadcrumbsItem cssFramework cssFramework breadcrumbs breadcrumbsItem `shouldBe` ""

                it "should render a breadcrumbs item marked as active" do
                    let breadcrumbsItem = baseBreadcrumbsItem {isActive = True}
                    let breadcrumbs = baseBreadcrumbs

                    styleBreadcrumbsItem cssFramework cssFramework breadcrumbs breadcrumbsItem `shouldBe` ""


shouldRenderTo renderFunction expectedHtml = Blaze.renderMarkup renderFunction `shouldBe` expectedHtml
