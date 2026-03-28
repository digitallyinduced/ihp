{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.View.Form.Fields
Description: Form field controls (text, number, checkbox, etc.)
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.Form.Fields where

import           IHP.HSX.ConvertibleStrings ()
import           IHP.HSX.Markup (Markup, ToHtml(..))
import           IHP.ModelSupport (InputValue, getModelName, inputValue)
import           IHP.Prelude
import           IHP.ValidationSupport
import           IHP.View.Classes ()
import           IHP.View.Types
import           IHP.ViewSupport

-- | Renders a text input field
--
-- >>> {textField #title}
-- <div class="form-group" id="form-group-post_title">
--     <label for="post_title">Title</label>
--     <input type="text" name="title" id="post_title" class="form-control" />
-- </div>
--
-- __Example:__
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {textField #title}
-- > |]
--
-- This will generate code like this:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <div class="form-group" id="form-group-post_title">
-- >         <label for="post_title">Title</label>
-- >         <input type="text" name="title" id="post_title" class="form-control" />
-- >     </div>
-- > </form>
--
-- __Help Texts:__
--
-- You can add a help text below a form control like this:
--
-- > {(textField #title) { helpText = "Max. 140 characters"} }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Title</label>
-- >
-- >     <input type="text" name="title" id="post_title" class="form-control" />
-- >     <small class="form-text text-muted">Max. 140 characters</small>
-- > </div>
--
--
-- __Custom Field Label Text:__
--
-- By default, the field name will be used as a label text. The camel case field name will be made more human-readable of course, so @contactName@ will turn to @Contact Name@, etc. Sometimes you want to change this auto-generated input label to something custom. Use @fieldLabel@ for that, like this:
--
-- > {(textField #title) { fieldLabel = "Post Title"} }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Post Title</label>
-- >     <input type="text" name="title" id="post_title" class="form-control" />
-- > </div>
--
--
-- __Custom CSS Classes:__
--
-- You can add custom CSS classes to the input and label for better styling. Set @fieldClass@ for adding a class to the input element and @labelClass@ for the label element:
--
-- > {(textField #title) { fieldClass="title-input", labelClass = "title-label" } }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label class="title-label" for="post_title">Title</label>
-- >     <input
-- >         type="text"
-- >         name="title"
-- >         id="post_title"
-- >         class="form-control title-input"
-- >     />
-- > </div>
--
-- Of course, the CSS classes for validation are still set as expected.
--
-- __Placeholder:__
--
-- > {(textField #title) { placeholder = "Enter your title ..." } }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Title</label>
-- >
-- >     <input
-- >         type="text"
-- >         name="title"
-- >         id="post_title"
-- >         placeholder="Enter your title ..."
-- >         class="form-control"
-- >     />
-- > </div>
--
--
-- __Required Fields:__
--
-- You can mark an input as required like this:
--
-- > {(textField #title) { required = True } }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Title</label>
-- >
-- >     <input
-- >         type="text"
-- >         name="title"
-- >         id="post_title"
-- >         required="required"
-- >         class="form-control"
-- >     />
-- > </div>
--
-- __Autofocus:__
--
-- You can mark an input with autofocus, to ensure it will be given the input focus on page load, like this:
--
-- > {(textField #title) { autofocus = True } }
--
-- This will generate code like this:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Title</label>
-- >
-- >     <input
-- >         type="text"
-- >         name="title"
-- >         id="post_title"
-- >         autofocus="autofocus"
-- >         class="form-control"
-- >     />
-- > </div>
textField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
textField field = FormField
        { fieldType = TextInput
        , fieldName = ?formContext.fieldNamePrefix <> cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldValue =  inputValue ((getField @fieldName model) :: value)
        , fieldInputId = cs (lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationViolation field model
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , additionalAttributes = []
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE textField #-}

-- | Renders a number input field
--
-- >>> {numberField #maxUsers}
-- <div class="form-group" id="form-group-company_max_users">
--     <label for="company_max_users">Max Users</label>
--     <input type="number" name="maxUsers" id="company_maxUsers" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
numberField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
numberField field = (textField field) { fieldType = NumberInput }
{-# INLINE numberField #-}

-- | Renders a URL input field
--
-- >>> {urlField #url}
-- <div class="form-group" id="form-group-company_url">
--     <label for="company_url">Url</label>
--     <input type="url" name="url" id="company_url" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
urlField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
urlField field = (textField field) { fieldType = UrlInput }
{-# INLINE urlField #-}

-- | Renders a textarea
--
-- >>> {textareaField #body}
-- <div class="form-group" id="form-group-post_body">
--     <label for="post_body">Body</label>
--     <textarea name="body" id="post_body" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
textareaField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
textareaField field = (textField field) { fieldType = TextareaInput }
{-# INLINE textareaField #-}

-- | Renders a color field
--
-- >>> {colorField #color}
-- <div class="form-group" id="form-group-post_color">
--     <label for="post_color">Color</label>
--     <input type="color" name="color" id="post_color" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
colorField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
colorField field = (textField field) { fieldType = ColorInput }
{-# INLINE colorField #-}


-- | Renders an email field
--
-- >>> {emailField #email}
-- <div class="form-group" id="form-group-user_email">
--     <label for="user_email">Email</label>
--     <input type="email" name="email" id="user_email" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
emailField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , InputValue value
    ) => Proxy fieldName -> FormField
emailField field = (textField field) { fieldType = EmailInput }
{-# INLINE emailField #-}

-- | Renders a date field
--
-- >>> {dateField #createdAt}
-- <div class="form-group" id="form-group-user_created_at">
--     <label for="user_createdAt">Created At</label>
--     <input type="date" name="createdAt" id="user_createdAt" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
--
-- Use @additionalAttributes@ and set @data-*@ attributes to control the flatpickr options.
--
-- > {(dateField #createdAt) {
-- >      additionalAttributes =
-- >          [ ("data-alt-format", "Y-m-d")
-- >          , ("data-min-date", "today")
-- >          ]
-- > }}
dateField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
dateField field = (textField field) { fieldType = DateInput }
{-# INLINE dateField #-}

-- | Renders a password field
--
-- >>> {passwordField #password}
-- <div class="form-group" id="form-group-user_password">
--     <label for="user_password">Password</label>
--     <input type="password" name="password" id="user_password" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
passwordField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Text
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
passwordField field = (textField field) { fieldType = PasswordInput }
{-# INLINE passwordField #-}

-- | Renders a @\<input type=\"datetime-local\"\/>@ field.
--
-- The field value is automatically formatted as @YYYY-MM-DDTHH:MM@ using
-- the 'DateTimeValue' typeclass, which ensures only 'UTCTime', 'LocalTime',
-- and their 'Maybe' variants are accepted. Using this on an incompatible type
-- (e.g. 'Bool') will produce a compile error.
--
-- >>> {dateTimeField #createdAt}
-- <div class="form-group" id="form-group-user_created_at">
--     <label for="user_createdAt">Created At</label>
--     <input type="datetime-local" name="createdAt" id="user_createdAt" class="form-control" />
-- </div>
--
-- The corresponding 'ParamReader' instances accept multiple formats on submission:
--
-- * @2020-11-08T12:03:35Z@ (ISO 8601 with seconds and Z)
-- * @2020-11-08T12:03:35@ (seconds without Z, produced when @step@ is set)
-- * @2020-11-08T12:03@ (default datetime-local, no seconds)
-- * @2020-11-08@ (date only)
--
-- See 'textField' for examples of possible form control options.
--
-- Use @additionalAttributes@ and set @data-*@ attributes to control the flatpickr options.
--
-- > {(dateTimeField #createdAt) {
-- >      additionalAttributes =
-- >          [ ("data-alt-format", "Y-m-d H:i")
-- >          , ("data-min-date", "today")
-- >          ]
-- > }}
dateTimeField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , DateTimeValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
dateTimeField alpha =
    (textField alpha)
        { fieldType = DateTimeInput
        , fieldValue = dateTimeValue ((getField @fieldName model) :: value)
        }
    where
        FormContext { model } = ?formContext
{-# INLINE dateTimeField #-}

-- | Provides a way to convert a time value to the format expected by
-- @\<input type=\"datetime-local\"\/>@, i.e. @YYYY-MM-DDTHH:MM@.
--
-- This ensures that 'dateTimeField' only accepts time-like types
-- ('UTCTime', 'LocalTime', and their 'Maybe' variants), giving a
-- compile error if used on unrelated types like 'Bool'.
class DateTimeValue a where
    dateTimeValue :: a -> Text
    default dateTimeValue :: FormatTime a => a -> Text
    dateTimeValue time = cs (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M" time)

instance DateTimeValue UTCTime
instance DateTimeValue LocalTime

instance DateTimeValue a => DateTimeValue (Maybe a) where
    dateTimeValue (Just time) = dateTimeValue time
    dateTimeValue Nothing = ""

-- | Renders a hidden field
--
-- >>> {hiddenField #projectId}
-- <input type="hidden" name="projectId" id="checkoutSession_projectId" class="form-control" />
--
-- The hidden field is by default rendered without a form group and without a label.
hiddenField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
hiddenField field = (textField field) { fieldType = HiddenInput, disableLabel = True, disableGroup = True, disableValidationResult = True }
{-# INLINE hiddenField #-}

-- | Renders a file field
--
-- >>> {fileField #profilePicture}
-- <input type="file" name="profilePicture" id="user_profilePicture" class="form-control" />
--
-- See 'textField' for examples of possible form control options.
fileField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
fileField field = (textField field) { fieldType = FileInput }
{-# INLINE fileField #-}

-- | Renders a checkbox field
--
-- >>> {checkboxField #active}
-- <div class="form-group" id="form-group-user_active">
--     <label for="user_active">Active</label>
--     <input type="checkbox" name="active" id="user_active" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
checkboxField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Bool
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
checkboxField field = FormField
        { fieldType = CheckboxInput
        , fieldName = cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldValue =  if getField @fieldName model then "yes" else "no"
        , fieldInputId = cs (lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationViolation field model
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , additionalAttributes = []
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE checkboxField #-}

instance ToHtml FormField where
    {-# INLINE toHtml #-}
    toHtml ::  FormField -> Markup
    toHtml formField@(FormField { cssFramework }) = styledFormField cssFramework cssFramework formField

instance ToHtml SubmitButton where
    {-# INLINE toHtml #-}
    toHtml submitButton@(SubmitButton { cssFramework }) = styledSubmitButton cssFramework cssFramework submitButton

-- | Renders a validation failure for a field. If the field passed all validation, no error is shown.
--
-- >>> {validationResult #email}
-- <div class="invalid-feedback">is not a valid email</div>
validationResult :: forall fieldName model fieldType.
    ( ?formContext :: FormContext model
    , HasField fieldName model fieldType
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue fieldType
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> Markup
validationResult field = styledValidationResult cssFramework cssFramework (textField field)
    where
        result = getValidationFailure field model
        model = ?formContext.model
        cssFramework = ?formContext.cssFramework

-- | Returns the validation failure for a field. If the field passed all validation, this returns 'Nothing'.
--
-- >>> {validationResultMaybe #email}
-- Just "is not a valid email"
validationResultMaybe :: forall fieldName model fieldType.
    ( ?formContext :: FormContext model
    , HasField fieldName model fieldType
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> Maybe Text
validationResultMaybe field = getValidationFailure field ?formContext.model
