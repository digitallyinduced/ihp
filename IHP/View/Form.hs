{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}
{-|
Module: IHP.View.Form
Description: 'IHP.View.Form.formFor' and all form controls
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.Form where

import IHP.Prelude
import           IHP.ValidationSupport
import           IHP.HSX.ConvertibleStrings ()
import IHP.ViewErrorMessages ()
import           IHP.ViewSupport
import qualified Text.Blaze.Html5                   as Html5
import IHP.HSX.ToHtml
import GHC.Types
import IHP.ModelSupport (getModelName, inputValue, isNew, Id', InputValue, didTouchField)
import IHP.HSX.QQ (hsx)
import IHP.View.Types
import IHP.View.Classes ()
import Network.Wai (pathInfo)
import IHP.Controller.Context

-- | Forms usually begin with a 'formFor' expression.
--
-- This is how a simple form can look like:
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {textField #title}
-- >     {textareaField #body}
-- >     {submitButton}
-- > |]
--
-- Calling this form from inside your HSX code will lead to the following HTML being generated:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <div class="form-group" id="form-group-post_title">
-- >         <label for="post_title">Title</label>
-- >         <input type="text" name="title" id="post_title" class="form-control" />
-- >     </div>
-- >
-- >     <div class="form-group" id="form-group-post_body">
-- >         <label for="post_body">Body</label>
-- >         <textarea name="body" id="post_body" class="form-control"></textarea>
-- >     </div>
-- >
-- >     <button class="btn btn-primary">Create Post</button>
-- > </form>
--
-- You can see that the form is submitted via @POST@. The form action has also been set by default to @/CreatePost@.
--
-- All inputs have auto-generated class names and ids for styling. Also, all @name@ attributes are set as expected.
--
-- __Field Values:__
--
-- A form control is always filled with the value of the given field when rendering. For example, given a post
--
-- > let post = Post { ..., title = "Hello World" }
--
-- Rendering this, the input value will be set like:
--
-- >>> {textField #title}
-- <input ... value="Hello World" />
--
-- __Validation:__
--
-- When rendering a record that has failed validation, the validation error message will be rendered automatically.
--
-- Given a post like this:
--
-- > let post = Post { ..., title = "" }
-- >     |> validateField #title nonEmpty
--
-- Rendering @{textField #title}@, the input will have the css class @is-invalid@ and an element with the error message will be rendered below the input:
--
-- > <div class="form-group" id="form-group-post_title">
-- >     <label for="post_title">Title</label>
-- >     <input
-- >         type="text"
-- >         name="title"
-- >         placeholder=""
-- >         id="post_title"
-- >         class="form-control is-invalid "
-- >     />
-- >     <div class="invalid-feedback">This field cannot be empty</div>
-- > </div>
formFor :: forall record. (
    ?context :: ControllerContext
    , ModelFormAction record
    , HasField "meta" record MetaBag
    ) => record -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor record formBody = formForWithOptions @record record (\c -> c) formBody
{-# INLINE formFor #-}

-- | Like 'formFor' but allows changing the underlying 'FormContext'
--
-- This is how you can render a form with a @id="post-form"@ id attribute and a custom @data-post-id@ attribute:
--
-- > renderForm :: Post -> Html
-- > renderForm post = formForWithOptions formOptions post [hsx|
-- >     {textField #title}
-- >     {textareaField #body}
-- >     {submitButton}
-- > |]
-- >
-- > formOptions :: FormContext Post -> FormContext Post
-- > formOptions formContext = formContext
-- >     |> set #formId "post-form"
-- >     |> set #customFormAttributes [("data-post-id", show formContext.model.id)]
--
formForWithOptions :: forall record. (
    ?context :: ControllerContext
    , ModelFormAction record
    , HasField "meta" record MetaBag
    ) => record -> (FormContext record -> FormContext record) -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formForWithOptions record applyOptions formBody = buildForm (applyOptions (createFormContext record) { formAction = modelFormAction record }) formBody
{-# INLINE formForWithOptions #-}

-- | Like 'formFor' but disables the IHP javascript helpers.
--
-- Use it like this:
--
-- > renderForm :: Post -> Html
-- > renderForm post = formForWithoutJavascript post [hsx|
-- >     {textField #title}
-- >     {textareaField #body}
-- >     {submitButton}
-- > |]
--
-- If you want to use this with e.g. a custom form action, remember that 'formForWithoutJavascript' is just a shortcut for 'formForWithOptions':
--
-- > renderForm :: Post -> Html
-- > renderForm post = formForWithOptions formOptions post [hsx|
-- >     {textField #title}
-- >     {textareaField #body}
-- >     {submitButton}
-- > |]
-- >
-- > formOptions :: FormContext Post -> FormContext Post
-- > formOptions formContext = formContext
-- >     |> set #formAction (pathTo BespokeNewPostAction)
-- >     |> set #disableJavascriptSubmission True
--
formForWithoutJavascript :: forall record. (
    ?context :: ControllerContext
    , ModelFormAction record
    , HasField "meta" record MetaBag
    ) => record -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formForWithoutJavascript record formBody = formForWithOptions @record record (\formContext -> formContext { disableJavascriptSubmission = True }) formBody
{-# INLINE formForWithoutJavascript #-}

-- | Allows a custom form action (form submission url) to be set
--
-- The URL where the form is going to be submitted to is specified in HTML using the form's @action@ attribute. When using 'formFor' the @action@ attribute is automatically set to the expected path.
--
-- E.g. given the below 'formFor' code, the @action@ is set to @/CreatePost@ or @/UpdatePost@:
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {textField #title}
-- >     {textareaField #body}
-- >     {submitButton}
-- > |]
--
-- To override the auto-generated @action@ attribute use the 'formFor\'' function:
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor' post "/my-custom-endpoint" [hsx||]
--
-- If you pass an action to that, you need to wrap it with 'pathTo':
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor' post (pathTo CreateDraftAction) [hsx||]
--
formFor' :: forall record. (
    ?context :: ControllerContext
    , HasField "meta" record MetaBag
    ) => record -> Text -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor' record action = buildForm (createFormContext record) { formAction = action }
{-# INLINE formFor' #-}

-- | Used by 'formFor' to make a new form context
createFormContext :: forall record. (
        ?context :: ControllerContext
        , HasField "meta" record MetaBag
        ) => record -> FormContext record
createFormContext record =
    FormContext
        { model = record
        , formAction = ""
        , formMethod = "POST"
        , cssFramework = theCSSFramework
        , formId = ""
        , formClass = if isNew record then "new-form" else "edit-form"
        , customFormAttributes = []
        , disableJavascriptSubmission = False
        , fieldNamePrefix = ""
        }
{-# INLINE createFormContext #-}

-- | Used by 'formFor' to render the form
buildForm :: forall model. (?context :: ControllerContext) => FormContext model -> ((?context :: ControllerContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
buildForm formContext inner = [hsx|
        <form
            method={formContext.formMethod}
            action={formContext.formAction}
            id={formContext.formId}
            class={formContext.formClass}
            data-disable-javascript-submission={formContext.disableJavascriptSubmission}
            {...formContext.customFormAttributes}
        >
            {formInner}
        </form>
    |]
        where
            formInner = let ?formContext = formContext in inner
{-# INLINE buildForm #-}

nestedFormFor :: forall fieldName childRecord parentRecord idType. (
    ?context :: ControllerContext
    , ?formContext :: FormContext parentRecord
    , HasField fieldName parentRecord [childRecord]
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName childRecord)
    , HasField "id" childRecord idType
    , InputValue idType
    , HasField "meta" childRecord MetaBag
    ) => Proxy fieldName -> ((?context :: ControllerContext, ?formContext :: FormContext childRecord) => Html5.Html) -> Html5.Html
nestedFormFor field nestedRenderForm = forEach children renderChild
    where
        parentFormContext :: FormContext parentRecord
        parentFormContext = ?formContext

        renderChild :: childRecord -> Html5.Html
        renderChild record = let ?formContext = buildNestedFormContext record in [hsx|
            {hiddenField #id}
            {nestedRenderForm}
        |]

        buildNestedFormContext :: childRecord -> FormContext childRecord
        buildNestedFormContext record = parentFormContext { model = record, fieldNamePrefix = symbolToText @fieldName <> "_" }

        children :: [childRecord]
        children = getField @fieldName ?formContext.model
{-# INLINE nestedFormFor #-}

-- | Renders a submit button
--
-- > <button class="btn btn-primary">Create Post</button>
--
-- __Example:__
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {submitButton}
-- > |]
--
-- This will generate code like this:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <button class="btn btn-primary">Create Post</button>
-- > </form>
--
-- __Custom Text__
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {submitButton { label = "Create it!" } }
-- > |]
--
-- This will generate code like this:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <button class="btn btn-primary">Create it!</button>
-- > </form>
--
-- __Custom Class__
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {submitButton { buttonClass = "create-button" } }
-- > |]
--
-- This will generate code like this:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <button class="btn btn-primary create-button">Create Post</button>
-- > </form>
--
-- __Disabled button__
--
-- > renderForm :: Post -> Html
-- > renderForm post = formFor post [hsx|
-- >     {submitButton { buttonDisabled = True } }
-- > |]
--
-- This will generate code like this:
--
-- > <form method="POST" action="/CreatePost" id="" class="new-form">
-- >     <button class="btn btn-primary create-button" disabled="disabled">Create Post</button>
-- > </form>
submitButton :: forall model. (?formContext :: FormContext model, HasField "meta" model MetaBag, KnownSymbol (GetModelName model)) => SubmitButton
submitButton =
    let
        modelName = IHP.ModelSupport.getModelName @model
        buttonText = modelName |> humanize -- We do this to turn 'Create ProjectTask' into 'Create Project Task'
        isNew = IHP.ModelSupport.isNew (model ?formContext)
    in SubmitButton
    { label = cs $ (if isNew then "Create " else "Save ") <> buttonText
    , buttonClass = mempty
    , buttonDisabled = False
    , cssFramework = ?formContext.cssFramework
    }
{-# INLINE submitButton #-}

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

-- | Renders an date field
--
-- >>> {dateField #createdAt}
-- <div class="form-group" id="form-group-user_created_at">
--     <label for="user_createdAt">Created At</label>
--     <input type="date" name="createdAt" id="user_createdAt" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
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

-- | Renders an password field
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

-- | Renders an date-time field
--
-- >>> {dateTimeField #createdAt}
-- <div class="form-group" id="form-group-user_created_at">
--     <label for="user_createdAt">Created At</label>
--     <input type="datetime-local" name="createdAt" id="user_createdAt" class="form-control" />
-- </div>
--
-- See 'textField' for examples of possible form control options.
dateTimeField :: forall fieldName model value.
    ( ?formContext :: FormContext model
    , HasField fieldName model value
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , InputValue value
    , KnownSymbol (GetModelName model)
    ) => Proxy fieldName -> FormField
dateTimeField alpha = (textField alpha) { fieldType = DateTimeInput }
{-# INLINE dateTimeField #-}

-- | Renders an hidden field
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

-- | Renders an file field
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

-- | Select inputs require you to pass a list of possible values to select.
--
-- > formFor project [hsx|
-- >     {selectField #userId users}
-- > |]
--
-- In the example above the variable users contains all the possible option values for the select.
--
-- You also need to define a instance @CanSelect User@:
--
-- > instance CanSelect User where
-- >     -- Here we specify that the <option> value should contain a `Id User`
-- >     type SelectValue User = Id User
-- >     -- Here we specify how to transform the model into <option>-value
-- >     selectValue user = user.id
-- >     -- And here we specify the <option>-text
-- >     selectLabel user = user.name
--
-- Given the above example, the rendered form will look like this:
--
-- > <!-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }] -->
-- > <form ...>
-- >     <select name="user_id">
-- >         <option value="1">Marc</option>
-- >         <option value="2">Andreas</option>
-- >     </select>
-- > </form>
--
-- If you want a certain value to be preselected, set the value in the controller. For example, to have the first user be preselected in the above example:
--
-- > action NewProjectAction = do
-- >     users <- query @User |> fetch
-- >     let userId = headMay users |> maybe def (.id)
-- >     let target = newRecord @Project |> set #userId userId
-- >     render NewView { .. }
selectField :: forall fieldName model item.
    ( ?formContext :: FormContext model
    , HasField fieldName model (SelectValue item)
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , CanSelect item
    , InputValue (SelectValue item)
    , Typeable model
    , Eq (SelectValue item)
    ) => Proxy fieldName -> [item] -> FormField
selectField field items = FormField
        { fieldType =
            let
                itemToTuple :: item -> (Text, Text)
                itemToTuple item = (selectLabel item, inputValue (selectValue item))
            in
                 SelectInput (map itemToTuple items)
        , fieldName = cs fieldName
        , fieldLabel = removeIdSuffix $ fieldNameToFieldLabel (cs fieldName)
        -- If the field is not touched, we don't want to render the value from the model
        -- so we force the user to select. If a value was explicitely set in the model, we
        -- render that value.
        , fieldValue = if IHP.ModelSupport.didTouchField field model || (not $ isNew model)
                    then inputValue (getField @fieldName model :: SelectValue item)
                    else ""
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
        , placeholder = "Please select"
        , required = False
        , autofocus = False
    }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE selectField #-}

-- | Radio require you to pass a list of possible values to select. We use the same mechanism as for for 'selectField'.
--
-- > formFor project [hsx|
-- >     {radioField #userId users}
-- > |]
--
-- In the example above the variable users contains all the possible option values for the radios.
--
-- You also need to define a instance @CanSelect User@:
--
-- > instance CanSelect User where
-- >     -- Here we specify that the <option> value should contain a `Id User`
-- >     type SelectValue User = Id User
-- >     -- Here we specify how to transform the model into <option>-value
-- >     selectValue user = user.id
-- >     -- And here we specify the <option>-text
-- >     selectLabel user = user.name
--
-- Given the above example, the rendered form will look like this (omitting classes for brevity):
--
-- > <!-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }] -->
-- > <form ...>
-- >     <fieldset>
-- >         <div>
-- >           <input type="radio" id="option1" value="1"/>
-- >           <label for="option1">Marc</label>
-- >         </div>
-- >         <div>
-- >           <input type="radio" id="option2" value="2"/>
-- >           <label for="option2">Andreas</label>
-- >         </div>
-- >     </fieldset>
-- > </form>
--
-- If you want a certain value to be preselected, set the value in the controller. For example, to have the first user be preselected in the above example:
--
-- > action NewProjectAction = do
-- >     users <- query @User |> fetch
-- >     let userId = headMay users |> maybe def (.id)
-- >     let target = newRecord @Project |> set #userId userId
-- >     render NewView { .. }
radioField :: forall fieldName model item.
    ( ?formContext :: FormContext model
    , HasField fieldName model (SelectValue item)
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
    , CanSelect item
    , InputValue (SelectValue item)
    , Typeable model
    , Eq (SelectValue item)
    ) => Proxy fieldName -> [item] -> FormField
radioField field items = (selectField field items)
    { fieldType =
        let
            itemToTuple :: item -> (Text, Text)
            itemToTuple item = (selectLabel item, inputValue (selectValue item))
        in
                RadioInput (map itemToTuple items)
    , placeholder = ""
    }
{-# INLINE radioField #-}

class CanSelect model where
    -- | Here we specify the type of the @<option>@ value, usually an @Id model@
    type SelectValue model :: GHC.Types.Type

    -- | Here we specify the <option>-text
    selectLabel :: model -> Text
    default selectLabel :: Show model => model -> Text
    selectLabel = tshow

    -- | Here we specify how to transform the model into @<option>@-value
    selectValue :: model -> SelectValue model
    default selectValue :: HasField "id" model (SelectValue model) => model -> SelectValue model
    selectValue = (.id)

instance ToHtml FormField where
    {-# INLINE toHtml #-}
    toHtml ::  FormField -> Html5.Html
    toHtml formField@(FormField { cssFramework }) = styledFormField cssFramework cssFramework formField

instance ToHtml SubmitButton where
    {-# INLINE toHtml #-}
    toHtml submitButton@(SubmitButton { cssFramework }) = styledSubmitButton cssFramework cssFramework submitButton

-- | Returns the form's action attribute for a given record.
class ModelFormAction record where
    modelFormAction :: (?context :: ControllerContext) => record -> Text

instance
    ( HasField "id" record (Id' (GetTableName record))
    , HasField "meta" record MetaBag
    , KnownSymbol (GetModelName record)
    , Show (Id' (GetTableName record))
    ) => ModelFormAction record where
    -- | Returns the form's action attribute for a given record.
    --
    -- Expects that AutoRoute is used. Otherwise you need to use @formFor'@ or specify
    -- a manual ModelFormAction instance.
    --
    -- We guess the form submit action based on the current url
    -- It's a @New..Action@ or @Edit..Action@. We guess the corresponding
    -- @Create..Action@ name or @Update..Action@ name based on the AutoRoute rules
    --
    -- In case the routing is not based on AutoRoute, a manual ModelFormAction instance needs
    -- to be defined
    modelFormAction record =
        let
            path = theRequest.pathInfo
            action = if isNew record
                then "Create" <> getModelName @record
                else "Update" <> getModelName @record <> "?" <> lcfirst (getModelName @record) <> "Id=" <> tshow record.id
        in
            init path
                |> (\path -> [""] <> (fromMaybe [] path) <> [action])
                |> intercalate "/"

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
    ) => Proxy fieldName -> Html
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
