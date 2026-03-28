{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}

module IHP.View.Types
( FormField (..)
, SubmitButton (..)
, FormContext (..)
, InputType (..)
, CSSFramework (..)
, BreadcrumbsView(..)
, PaginationView(..)
, HtmlWithContext
, Layout
)
where

import Prelude
import Data.Text (Text)
import Data.ByteString (ByteString)
import IHP.HaskellSupport (SetField(..))
import IHP.HSX.Markup (Markup)
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
import IHP.ModelSupport.Types (Violation)
import IHP.Breadcrumb.Types
import IHP.Pagination.Types
import Network.Wai (Request)


type HtmlWithContext context = (?context :: context, ?request :: Request) => Markup

-- | A layout is just a function taking a view and returning a new view.
--
-- __Example:__ A very basic html layout.
--
-- > myLayout :: Layout
-- > myLayout view = [hsx|
-- >     <html>
-- >         <body>
-- >             {view}
-- >         </body>
-- >     </html>
-- > |]
type Layout = Markup -> Markup

data FormField = FormField
    { fieldType :: !InputType
    , fieldName :: !Text
    , fieldLabel :: !Text
    , fieldValue :: !Text
    , fieldInputId :: !Text
    , validatorResult :: !(Maybe Violation)
    , additionalAttributes :: [(Text, Text)]
    , fieldClass :: !Text
    , labelClass :: !Text
    , disabled :: !Bool
    , disableLabel :: !Bool
    , disableGroup :: !Bool
    , disableValidationResult :: !Bool
    , cssFramework :: CSSFramework
    , helpText :: !Text
    , placeholder :: !Text
    , required :: Bool
    , autofocus :: Bool
    }

data SubmitButton = SubmitButton
    { label :: Markup
    , buttonClass :: Text
    , buttonDisabled :: Bool
    , cssFramework :: CSSFramework
    }

data FormContext model = FormContext
    { model :: model -- ^ The record this form is based on
    , formAction :: !Text -- ^ Url where the form is submitted to
    , formMethod :: !Text -- ^ Usually "POST", sometimes set to "GET"
    , cssFramework :: !CSSFramework
    , formClass :: !Text -- ^ In the generated HTML, the @class@  attribute will be set to this value
    , formId :: !Text -- ^ In the generated HTML, the @id@ attribute will be set to this value
    , disableJavascriptSubmission :: !Bool -- ^ When set to True, the IHP helpers.js will not submit the form using ajax
    , customFormAttributes :: ![(Text, Text)] -- ^ Attach custom HTML attributes here
    , fieldNamePrefix :: !Text -- ^ Used by nested forms to preprend the nested field name to the field name
    }
instance SetField "model" (FormContext record) record where setField value record = record { model = value }
instance SetField "formAction" (FormContext record) Text where setField value record = record { formAction = value }
instance SetField "formMethod" (FormContext record) Text where setField value record = record { formMethod = value }
instance SetField "cssFramework" (FormContext record) CSSFramework where setField value record = record { cssFramework = value }
instance SetField "formClass" (FormContext record) Text where setField value record = record { formClass = value }
instance SetField "formId" (FormContext record) Text where setField value record = record { formId = value }
instance SetField "disableJavascriptSubmission" (FormContext record) Bool where setField value record = record { disableJavascriptSubmission = value }
instance SetField "customFormAttributes" (FormContext record) [(Text, Text)] where setField value record = record { customFormAttributes = value }

data InputType
    = TextInput
    | NumberInput
    | UrlInput
    | CheckboxInput
    | ColorInput
    | EmailInput
    | HiddenInput
    | TextareaInput
    | DateInput
    | DateTimeInput
    | PasswordInput
    | SelectInput { options :: ![(Text, Text)] }
    | RadioInput { options :: ![(Text, Text)] }
    | FileInput


data BreadcrumbsView = BreadcrumbsView { breadcrumbItems :: !Markup }

data PaginationView =
    PaginationView
    { cssFramework :: !CSSFramework
    , pagination :: !Pagination
    -- Function used to get the page URL.
    , pageUrl :: Int -> ByteString
    -- Previous page link.
    , linkPrevious :: !Markup
    -- Next page link.
    , linkNext :: !Markup
    -- The page and dot dot as rendered by `styledPaginationPageLink` and `styledPaginationDotDot`.
    , pageDotDotItems :: !Markup
    -- Selector changing the number of allowed items per page.
    , itemsPerPageSelector :: !Markup
    }

-- | Render functions to render with Bootstrap, Tailwind CSS etc.
-- We call this functions with the cssFramework passed to have late binding (like from OOP languages).
-- Here's an explanation breaking it down, step by step
--
-- > renderedHtml = styledPagination theCSSFramework theCSSFramework paginationView
--
-- Can also be written using get:
--
-- > renderedHtml = (theCSSFramework.styledPagination) theCSSFramework paginationView
--
-- That's important to understand here. We get a 'styledPagination' function using 'theCSSFramework.styledPagination'.
-- Next, we apply 'theCSSFramework' to that function. We do that so because the 'styledPagination' function internally
-- might want to call other functions of the CSSFramework type. But we might want to override some functions of the CSSFramework.
--
-- Here's an example of how it would look if we don't pass this a second time, and it's shortcomings.
-- Let's assume 'styledPagination' is calling a 'styledButton':
--
-- > data CSSFramework = CSSFramework { styledPagination :: Html, styledButton :: Html }
-- >
-- > bootstrapCSS = CSSFramework { styledPagination, styledButton }
-- >    where
-- >        styledPagination = [hsx|<div>{styledButton}</div>|]
-- >        styledButton = [hsx|<button style="color: red">button</button>|]]
-- >
-- > myPage = [hsx|{styledPagination bootstrapCSS}|]
--
-- So far all seems fine. But now let's say we would like to override the button styling, and change the button to green instead of red:
--
-- > customCSS = bootstrapCSS { styledButton = [hsx|<button style="color: green">button</button>|]]  }
-- >
-- > myPage = [hsx|{styledPagination customCSS}|]
--
-- Now, when we render the 'myPage' we will get '<div><button style="color: red">button</button></div>' (a red button, while our customCSS specified it should be green).
--
-- Our way to fix this is to "late-bind" the calls, by manually passing around a CSSFramework. Here we added that second 'CSSFramework' to all functions.
-- Notice how 'styledPagination' is getting the correct 'styledButton' by calling 'cssFramework.styledButton':
--
-- > data CSSFramework = CSSFramework { styledPagination :: CSSFramework -> Html, styledButton :: CSSFramework -> Html }
-- >
-- > bootstrapCSS = CSSFramework { styledPagination, styledButton }
-- >    where
-- >        styledPagination cssFramework = [hsx|<div>{cssFramework.styledButton}</div>|]
-- >        styledButton cssFramework = [hsx|<button style="color: red">button</button>|]]
-- >
-- > myPage = [hsx|{styledPagination bootstrapCSS bootstrapCSS}|]
--
-- Now, with this second 'CSSFramework' in place we can customize it again:
--
-- > customCSS = bootstrapCSS { styledButton = \cssFramework -> [hsx|<button style="color: green">button</button>|]]  }
-- >
-- > myPage = [hsx|{styledPagination customCSS customCSS}|]
data CSSFramework = CSSFramework
    { styledFlashMessage :: CSSFramework -> FlashMessage -> Markup
    , styledFlashMessages :: CSSFramework -> [FlashMessage] -> Markup
    -- | Renders the full form field calling other functions below
    , styledFormField :: CSSFramework -> FormField -> Markup
    , styledTextFormField :: CSSFramework -> Text -> FormField -> Markup -> Markup
    , styledTextareaFormField :: CSSFramework -> FormField -> Markup -> Markup
    , styledCheckboxFormField :: CSSFramework -> FormField -> Markup -> Markup
    , styledSelectFormField :: CSSFramework -> FormField -> Markup -> Markup
    , styledRadioFormField :: CSSFramework -> FormField -> Markup -> Markup
    , styledFormGroup :: CSSFramework -> Text -> Markup -> Markup
    -- | The primary form submit button
    , styledSubmitButton :: CSSFramework -> SubmitButton -> Markup
    -- | Class for the primary form submit button
    , styledSubmitButtonClass :: Text
    -- | Renders the help text below an input field. Used with @[hsx|{(textField #firstname) { helpText = "Your first name" } }|]@
    , styledFormFieldHelp :: CSSFramework -> FormField -> Markup
    -- | First class attached to @<input/>@ elements, e.g. @<input class="form-control"/>@
    , styledInputClass :: CSSFramework -> FormField -> Text
    -- | When the form validation failed, invalid inputs will have this class
    , styledInputInvalidClass :: CSSFramework -> FormField -> Text
    -- | Class applied to the div wrapping the label and input, e.g. @"form-group"@
    , styledFormGroupClass :: Text
    -- | Class applied to @<label>@ elements in form fields, e.g. @"form-label"@
    , styledLabelClass :: Text
    -- | Elements that containers the validation error message for a invalid form field
    , styledValidationResult :: CSSFramework -> FormField -> Markup
    -- | Class name for container of validation error message
    , styledValidationResultClass :: Text
    -- | Renders a the entire pager, with all its elements.
    , styledPagination :: CSSFramework -> PaginationView -> Markup
    -- | The pagination's previous link
    , styledPaginationLinkPrevious :: CSSFramework -> Pagination -> ByteString -> Markup
    -- | The pagination's next link
    , styledPaginationLinkNext :: CSSFramework -> Pagination -> ByteString -> Markup
    -- | Render the pagination links
    , styledPaginationPageLink :: CSSFramework -> Pagination -> ByteString -> Int -> Markup
    -- | Render the dots between pagination numbers (e.g. 5 6 ... 7 8)
    , styledPaginationDotDot :: CSSFramework -> Pagination -> Markup
    -- | Render the items per page selector for pagination.
    -- Note the (Int -> ByteString), we are passing the pageUrl function, so anyone that would like to override
    -- it the selector with different items per page could still use the pageUrl function to get the correct URL.
    , styledPaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Markup
    -- | Renders an entire breadcrumbs element.
    , styledBreadcrumb :: CSSFramework -> [BreadcrumbItem] -> BreadcrumbsView -> Markup
    -- | Render a single breadcrumb item. We pass the entire list of breadcrumbs, in case an item may change based on that list.
    -- The 'Bool' indicates if item is the last one.
    , styledBreadcrumbItem :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbItem -> Bool -> Markup
    }
