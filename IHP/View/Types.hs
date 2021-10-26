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
, PaginationView(..)
, HtmlWithContext
, Layout
)
where

import IHP.Prelude hiding (div)
import qualified Text.Blaze.Html5 as Blaze
import IHP.FlashMessages.Types
import IHP.ModelSupport (Violation)
import IHP.Breadcrumbs.Types
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
    , fieldName :: !Blaze.AttributeValue
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
--
-- We call this functions with the cssFramework passed to have late binding (like from OOP languages)
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
    , stylePaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
    -- | Render the optional first breadcrumb, which leads to the homepage.
    , styleHomepageBreadcrumbsItem :: CSSFramework -> [ BreadcrumbsItem ]-> Blaze.Html
    -- | Render a breadcumbs item. We pass the entire list of breadcrumbs, in case an item may change based on that list.
    , styleBreadcrumbsItem :: CSSFramework -> [ BreadcrumbsItem ]-> BreadcrumbsItem -> Blaze.Html
    }
