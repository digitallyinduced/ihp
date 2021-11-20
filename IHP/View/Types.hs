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

import IHP.Prelude hiding (div)
import qualified Text.Blaze.Html5 as Blaze
import IHP.FlashMessages.Types
import IHP.ModelSupport (Violation)
import IHP.Breadcrumb.Types
import IHP.Pagination.Types


type HtmlWithContext context = (?context :: context) => Blaze.Html

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
type Layout = Blaze.Html -> Blaze.Html

data FormField = FormField
    { fieldType :: !InputType
    , fieldName :: !Text
    , fieldLabel :: !Text
    , fieldValue :: !Text
    , fieldInputId :: !Text
    , validatorResult :: !(Maybe Violation)
    , fieldInput :: !(FormField -> Blaze.Html)
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
    { label :: Blaze.Html
    , buttonClass :: Text
    , cssFramework :: CSSFramework
    }

data FormContext model = FormContext
    { model :: model -- ^ The record this form is based on
    , formAction :: !Text -- ^ Url where the form is submitted to
    , cssFramework :: !CSSFramework
    , formClass :: !Text -- ^ In the generated HTML, the @class@  attribute will be set to this value
    , formId :: !Text -- ^ In the generated HTML, the @id@ attribute will be set to this value
    , disableJavascriptSubmission :: !Bool -- ^ When set to True, the IHP helpers.js will not submit the form using ajax
    , customFormAttributes :: ![(Text, Text)] -- ^ Attach custom HTML attributes here
    }
instance SetField "model" (FormContext record) record where setField value record = record { model = value }
instance SetField "formAction" (FormContext record) Text where setField value record = record { formAction = value }
instance SetField "cssFramework" (FormContext record) CSSFramework where setField value record = record { cssFramework = value }
instance SetField "formClass" (FormContext record) Text where setField value record = record { formClass = value }
instance SetField "formId" (FormContext record) Text where setField value record = record { formId = value }
instance SetField "disableJavascriptSubmission" (FormContext record) Bool where setField value record = record { disableJavascriptSubmission = value }
instance SetField "customFormAttributes" (FormContext record) [(Text, Text)] where setField value record = record { customFormAttributes = value }

data InputType
    = TextInput
    | NumberInput
    | CheckboxInput
    | ColorInput
    | EmailInput
    | HiddenInput
    | TextareaInput
    | DateInput
    | DateTimeInput
    | PasswordInput
    | SelectInput { options :: ![(Text, Text)] }
    | FileInput


data BreadcrumbsView = BreadcrumbsView { breadcrumbItems :: !Blaze.Html }

data PaginationView =
    PaginationView
    { cssFramework :: !CSSFramework
    , pagination :: !Pagination
    -- Function used to get the page URL.
    , pageUrl :: Int -> ByteString
    -- Previous page link.
    , linkPrevious :: !Blaze.Html
    -- Next page link.
    , linkNext :: !Blaze.Html
    -- The page and dot dot as rendered by `styledPaginationPageLink` and `styledPaginationDotDot`.
    , pageDotDotItems :: !Blaze.Html
    -- Selector changing the number of allowed items per page.
    , itemsPerPageSelector :: !Blaze.Html
    }

-- | Render functions to render with Bootstrap, Tailwind CSS etc.
-- We call this functions with the cssFramework passed to have late binding (like from OOP languages).
-- Here's an explanation breaking it down, step by step
--
-- > renderedHtml = styledPagination theCSSFramework theCSSFramework paginationView
--
-- Can also be written using get:
--
-- > renderedHtml = (get #styledPagination theCSSFramework) theCSSFramework paginationView
--
-- That's important to understand here. We get a 'styledPagination' function using 'get #styledPagination theCSSFramework'.
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
-- Notice how 'styledPagination' is getting the correct 'styledButton' by calling 'get #styledButton cssFramework':
--
-- > data CSSFramework = CSSFramework { styledPagination :: CSSFramework -> Html, styledButton :: CSSFramework -> Html }
-- >
-- > bootstrapCSS = CSSFramework { styledPagination, styledButton }
-- >    where
-- >        styledPagination cssFramework = [hsx|<div>{get #styledButton cssFramework}</div>|]
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
    { styledFlashMessage :: CSSFramework -> FlashMessage -> Blaze.Html
    , styledFlashMessages :: CSSFramework -> [FlashMessage] -> Blaze.Html
    -- | Renders the full form field calling other functions below
    , styledFormField :: CSSFramework -> FormField -> Blaze.Html
    -- | The primary form submit button
    , styledSubmitButton :: CSSFramework -> SubmitButton -> Blaze.Html
    -- | Class for the primary form submit button
    , styledSubmitButtonClass :: Text
    -- | Renders the help text below an input field. Used with @[hsx|{(textField #firstname) { helpText = "Your first name" } }|]@
    , styledFormFieldHelp :: CSSFramework -> FormField -> Blaze.Html
    -- | First class attached to @<input/>@ elements, e.g. @<input class="form-control"/>@
    , styledInputClass :: FormField -> Text
    -- | When the form validation failed, invalid inputs will have this class
    , styledInputInvalidClass :: FormField -> Text
    -- | Class applied to the div wrapping the label and input, e.g. @"form-group"@
    , styledFormGroupClass :: Text
    -- | Elements that containers the validation error message for a invalid form field
    , styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
    -- | Class name for container of validation error message
    , styledValidationResultClass :: Text
    -- | Renders a the entire pager, with all its elements.
    , styledPagination :: CSSFramework -> PaginationView -> Blaze.Html
    -- | The pagination's previous link
    , styledPaginationLinkPrevious :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
    -- | The pagination's next link
    , styledPaginationLinkNext :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
    -- | Render the pagination links
    , styledPaginationPageLink :: CSSFramework -> Pagination -> ByteString -> Int -> Blaze.Html
    -- | Render the dots between pagination numbers (e.g. 5 6 ... 7 8)
    , styledPaginationDotDot :: CSSFramework -> Pagination -> Blaze.Html
    -- | Render the items per page selector for pagination.
    -- Note the (Int -> ByteString), we are passing the pageUrl function, so anyone that would like to override
    -- it the selector with different items per page could still use the pageUrl function to get the correct URL.
    , styledPaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
    -- | Renders an entire breadcrumbs element.
    , styledBreadcrumb :: CSSFramework -> [BreadcrumbItem] -> BreadcrumbsView -> Blaze.Html
    -- | Render a single breadcrumb item. We pass the entire list of breadcrumbs, in case an item may change based on that list.
    -- The 'Bool' indicates if item is the last one.
    , styledBreadcrumbItem :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbItem -> Bool -> Blaze.Html
    }
