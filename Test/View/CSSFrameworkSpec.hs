{-|
Module: Test.View.CSSFrameworkSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.View.CSSFrameworkSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Controller.Context
import IHP.FrameworkConfig as FrameworkConfig
import Control.Exception
import IHP.Controller.RequestContext
import IHP.View.Types
import IHP.View.CSSFramework
import IHP.FlashMessages.Types
import IHP.Controller.Session
import qualified Text.Blaze.Renderer.Text as Blaze
import qualified Text.Blaze.Html5 as H
import IHP.HSX.QQ (hsx)
import IHP.ModelSupport
import IHP.Breadcrumb.Types
import IHP.Breadcrumb.ViewFunctions (breadcrumbLink, breadcrumbLinkExternal, breadcrumbText, renderBreadcrumb)
import IHP.Pagination.Types
import qualified IHP.Prelude as Text (isInfixOf)
import qualified Data.TMap as TypeMap
import qualified Network.Wai as Wai
import IHP.Pagination.ViewFunctions (renderPagination)

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
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"> </div>"

                it "should render with disabled" do
                    let textField = baseTextField { disabled = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\" disabled=\"disabled\"> </div>"

                it "should render a validation error" do
                    let textField = baseTextField { validatorResult = Just (TextViolation "should not be empty") }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control is-invalid\"><div class=\"invalid-feedback\">should not be empty</div> </div>"

                it "should render with disableLabel = True" do
                    let textField = baseTextField { disableLabel = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"> </div>"

                it "should render with disableGroup = True" do
                    let textField = baseTextField { disableGroup = True }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"> "

                it "should render with help text" do
                    let textField = baseTextField { helpText = "Enter your first name" }
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-fname\"><label class=\"\" for=\"fname\">First name:</label><input type=\"text\" name=\"firstname\" placeholder=\"Your firstname\" id=\"fname\" class=\"form-control\"> <small class=\"form-text text-muted\">Enter your first name</small></div>"


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
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\"><label class=\"form-check-label\" for=\"user_is_active\">Is Active</label>  </div></div>"

                it "should render with disabled" do
                    let checkbox = baseCheckbox { disabled = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\" checked=\"checked\" disabled=\"disabled\"><input type=\"hidden\" name=\"is_active\" value=\"off\"><label class=\"form-check-label\" for=\"user_is_active\">Is Active</label>  </div></div>"

                it "should render without checked" do
                    let checkbox = baseCheckbox { fieldValue = "no" }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\"><input type=\"hidden\" name=\"is_active\" value=\"off\"><label class=\"form-check-label\" for=\"user_is_active\">Is Active</label>  </div></div>"

                it "should render with disableLabel = True" do
                    let checkbox = baseCheckbox { disableLabel = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><div><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\">  </div></div></div>"

                it "should render with disableGroup = True" do
                    let checkbox = baseCheckbox { disableGroup = True }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-check\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\"><label class=\"form-check-label\" for=\"user_is_active\">Is Active</label>  </div>"

                it "should render with help text" do
                    let checkbox = baseCheckbox { helpText = "Is the user active?" }
                    styledFormField cssFramework cssFramework checkbox `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-user_is_active\"><div class=\"form-check\"><input type=\"checkbox\" name=\"is_active\" class=\"form-check-input\" id=\"user_is_active\" checked=\"checked\"><input type=\"hidden\" name=\"is_active\" value=\"off\"><label class=\"form-check-label\" for=\"user_is_active\">Is Active</label>  <small class=\"form-text text-muted\">Is the user active?</small></div></div>"

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
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\"><option selected=\"selected\" disabled=\"disabled\">Please select</option> <option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select>  </div>"

                it "should render with disabled" do
                    let select = baseSelect { disabled = True }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\" disabled=\"disabled\"><option selected=\"selected\" disabled=\"disabled\">Please select</option> <option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select>  </div>"

                it "should render with selected" do
                    let select = baseSelect { fieldValue = "b" }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"b\"><option disabled=\"disabled\">Please select</option> <option value=\"a\">First Value</option><option value=\"b\" selected=\"selected\">Second Value</option></select>  </div>"

                it "should render with custom placeholder" do
                    let select = baseSelect { placeholder = "Pick something" }
                    styledFormField cssFramework cssFramework select `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-project_user_id\"><label class=\"\" for=\"project_user_id\">User</label><select name=\"user_id\" id=\"project_user_id\" class=\"form-control\" value=\"\"><option selected=\"selected\" disabled=\"disabled\">Pick something</option> <option value=\"a\">First Value</option><option value=\"b\">Second Value</option></select>  </div>"
            
            describe "textarea field" do
                let baseTextField = FormField
                        { fieldType = TextareaInput
                        , fieldName = "body"
                        , fieldLabel = "Body:"
                        , fieldValue = "Hello\nWorld!"
                        , fieldInputId = "body"
                        , validatorResult = Nothing
                        , fieldInput = \formField -> H.textarea (cs (fieldValue formField))
                        , fieldClass = ""
                        , labelClass = ""
                        , disabled = False
                        , disableLabel = False
                        , disableGroup = False
                        , disableValidationResult = False
                        , cssFramework = cssFramework
                        , helpText = ""
                        , placeholder = "Describe your issue"
                        , required = False
                        , autofocus = False
                        }
                it "should render" do
                    let textField = baseTextField
                    styledFormField cssFramework cssFramework textField `shouldRenderTo` "<div class=\"form-group\" id=\"form-group-body\"><label class=\"\" for=\"body\">Body:</label><textarea name=\"body\" placeholder=\"Describe your issue\" id=\"body\" class=\"form-control\">Hello\nWorld!</textarea></div>"

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
                    styledPaginationItemsPerPageSelector cssFramework cssFramework pagination (\n -> cs $ "https://example.com?maxItems=" <> (show n)) `shouldRenderTo` "<option value=\"10\" data-url=\"https://example.com?maxItems=10\">10 items per page</option><option value=\"20\" data-url=\"https://example.com?maxItems=20\">20 items per page</option><option value=\"50\" data-url=\"https://example.com?maxItems=50\">50 items per page</option><option value=\"100\" data-url=\"https://example.com?maxItems=100\">100 items per page</option><option value=\"200\" data-url=\"https://example.com?maxItems=200\">200 items per page</option>"

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

                it "should render the pagination if there are enough elements" do
                    let pagination = Pagination
                            { pageSize = 5
                            , totalItems = 15
                            , currentPage = 1
                            , window = 3
                            }

                    context <- createControllerContextWithCSSFramework cssFramework
                    let ?context = context

                    let render = Blaze.renderMarkup $ renderPagination pagination
                    Text.isInfixOf "<nav aria-label=\"Page Navigator\"" (cs render) `shouldBe` True


                it "should not render the pagination if there aren't enough elements" do
                    let pagination = Pagination
                            { pageSize = 10
                            , totalItems = 5
                            , currentPage = 1
                            , window = 3
                            }

                    context <- createControllerContextWithCSSFramework cssFramework
                    let ?context = context

                    renderPagination pagination `shouldRenderTo` mempty


            describe "breadcrumbs" do
                it "should render a breadcrumb item with no link" do
                    let breadcrumbItem = breadcrumbText "First item"
                    let breadcrumbs = [breadcrumbItem]

                    styledBreadcrumbItem cssFramework cssFramework breadcrumbs breadcrumbItem True `shouldRenderTo` "<li class=\"breadcrumb-item active\">First item</li>"

                it "should render a breadcrumb item with external link" do
                    let breadcrumbItem = breadcrumbLinkExternal "First item" "https://example.com"
                    let breadcrumbs = [breadcrumbItem]

                    styledBreadcrumbItem cssFramework cssFramework breadcrumbs breadcrumbItem True `shouldRenderTo` "<li class=\"breadcrumb-item active\"><a href=\"https://example.com\">First item</a></li>"

                it "should render a breadcrumb item as non-active" do
                    let breadcrumbItem = breadcrumbText "First item"
                    let breadcrumbs = [breadcrumbItem]

                    styledBreadcrumbItem cssFramework cssFramework breadcrumbs breadcrumbItem False `shouldRenderTo` "<li class=\"breadcrumb-item\">First item</li>"

                it "should render the wrapping breadcrumb and last item as active" do
                    let breadcrumbs = [breadcrumbText "First item", breadcrumbText "Last item"]

                    context <- createControllerContextWithCSSFramework cssFramework
                    let ?context = context

                    renderBreadcrumb breadcrumbs `shouldRenderTo` "<nav><ol class=\"breadcrumb\"><li class=\"breadcrumb-item\">First item</li><li class=\"breadcrumb-item active\">Last item</li></ol></nav>"

                it "should support show of BreadcrumbItem" do
                    let breadcrumbItem = breadcrumbText "First item"
                    show breadcrumbItem `shouldBe` "{ breadcrumbLabel = \"First item\", url = Nothing }"



shouldRenderTo renderFunction expectedHtml = Blaze.renderMarkup renderFunction `shouldBe` expectedHtml

{-| Mock a Controller context with CSSFramework.
-}
createControllerContextWithCSSFramework :: Typeable option => option -> IO ControllerContext
createControllerContextWithCSSFramework cssFramework = do
    frameworkConfig <- FrameworkConfig.buildFrameworkConfig do
                option cssFramework
    let requestBody = FormBody { params = [], files = [] }
    let request = Wai.defaultRequest
    let requestContext = RequestContext { request, respond = error "respond", requestBody, vault = error "vault", frameworkConfig = frameworkConfig }
    pure FrozenControllerContext { requestContext, customFields = TypeMap.empty }