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
import           IHP.View.ConvertibleStrings ()
import           IHP.ViewErrorMessages
import           IHP.ViewSupport
import qualified Text.Blaze.Html5                   as Html5
import IHP.HtmlSupport.ToHtml
import IHP.NameSupport
import GHC.Types
import IHP.RouterSupport hiding (get)
import IHP.ModelSupport (getModelName, inputValue, isNew, GetModelName, Id', NormalizeModel, MetaBag, InputValue)
import IHP.HtmlSupport.QQ (hsx)
import IHP.View.Types
import IHP.View.Classes 
import qualified Network.Wai as Wai
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
formFor :: forall record parent id application. (
    ?context :: ControllerContext
    , Eq record
    , Typeable record
    , ModelFormAction application record
    , HasField "id" record id
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    ) => record -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor record = buildForm (createFormContext record) { formAction = modelFormAction @application record }
{-# INLINE formFor #-}

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
formFor' :: forall record parent id application. (
    ?context :: ControllerContext
    , Eq record
    , Typeable record
    , HasField "id" record id
    , HasField "meta" record MetaBag
    , Default id
    , Eq id
    ) => record -> Text -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Html5.Html) -> Html5.Html
formFor' record action = buildForm (createFormContext record) { formAction = action }
{-# INLINE formFor' #-}

-- | Used by 'formFor' to make a new form context
createFormContext :: forall record viewContext parent id application. (
        ?context :: ControllerContext
        , Eq record
        , Typeable record
        , HasField "id" record id
        , HasField "meta" record MetaBag
        ) => record -> FormContext record
createFormContext record =
    FormContext
        { model = record
        , formAction = ""
        , cssFramework = theCSSFramework
        }
{-# INLINE createFormContext #-}

-- | Used by 'formFor' to render the form
buildForm :: forall model  parent id. (?context :: ControllerContext, HasField "id" model id, Default id, Eq id) => FormContext model -> ((?context :: ControllerContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
buildForm formContext inner =
    let
        theModel = model formContext
        action = formAction formContext
        isNewRecord = IHP.ModelSupport.isNew theModel
        formId = if isNewRecord then "" else formAction formContext
        formClass :: Text = if isNewRecord then "new-form" else "edit-form"
        formInner = let ?formContext = formContext in inner
    in
        [hsx|<form method="POST" action={action} id={formId} class={formClass}>{formInner}</form>|]
{-# INLINE buildForm #-}

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
submitButton :: forall model id. (?formContext :: FormContext model, HasField "id" model id, KnownSymbol (GetModelName model), Eq id, Default id) => SubmitButton
submitButton =
    let
        modelName = IHP.ModelSupport.getModelName @model
        isNew = IHP.ModelSupport.isNew (model ?formContext)
    in SubmitButton
    { label = cs $ (if isNew then "Create " else "Save ") <> modelName
    , buttonClass = mempty
    , cssFramework = get #cssFramework ?formContext
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
        , fieldName = cs fieldName
        , fieldLabel = fieldNameToFieldLabel (cs fieldName)
        , fieldValue =  inputValue ((getField @fieldName model) :: value)
        , fieldInputId = cs (lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const Html5.input
        , cssFramework = get #cssFramework ?formContext
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
textareaField field = (textField field) { fieldType = TextareaInput, fieldInput = \formField -> Html5.textarea (cs (fieldValue formField)) }
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
emailField :: forall fieldName model.
    ( ?formContext :: FormContext model
    , HasField fieldName model Text
    , HasField "meta" model MetaBag
    , KnownSymbol fieldName
    , KnownSymbol (GetModelName model)
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
hiddenField field = (textField field) { fieldType = HiddenInput }
{-# INLINE hiddenField #-}

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
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const Html5.input
        , cssFramework = get #cssFramework ?formContext
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
-- >     selectValue = get #id
-- >     -- And here we specify the <option>-text
-- >     selectLabel = get #name
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
-- >     let userId = headMay users |> maybe def (get #id)
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
        , fieldValue = inputValue ((getField @fieldName model :: SelectValue item))
        , fieldInputId = cs (lcfirst (getModelName @model) <> "_" <> cs fieldName)
        , validatorResult = getValidationFailure field model
        , fieldClass = ""
        , labelClass = ""
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , fieldInput = const (Html5.select mempty)
        , cssFramework = get #cssFramework ?formContext
        , helpText = ""
        , placeholder = "Please select"
        , required = False
        , autofocus = False
    }
    where
        fieldName = symbolVal field
        FormContext { model } = ?formContext
{-# INLINE selectField #-}

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
    selectValue = getField @"id"

instance ToHtml FormField where
    {-# INLINE toHtml #-}
    toHtml ::  FormField -> Html5.Html
    toHtml formField@(FormField { cssFramework }) = styledFormField cssFramework cssFramework formField

instance ToHtml SubmitButton where
    {-# INLINE toHtml #-}
    toHtml submitButton@(SubmitButton { cssFramework }) = styledSubmitButton cssFramework cssFramework submitButton

-- | Returns the form's action attribute for a given record.
class ModelFormAction application record where
    modelFormAction :: (?context :: ControllerContext) => record -> Text

instance (
    HasField "id" record id
    , Eq id
    , Default id
    , KnownSymbol (GetModelName record)
    , Show id
    ) => ModelFormAction application record where
    -- | Returns the form's action attribute for a given record.
    --
    -- Expects that AutoRoute is used. Otherwise you need to use @formFor'@ or specify
    -- a manual ModelFormAction instance.
    --
    -- We guess the form submitt action based on the current url
    -- It's a @New..Action@ or @Edit..Action@. We guess the corresponding
    -- @Create..Action@ name or @Update..Action@ name based on the AutoRoute rules
    --
    -- In case the routing is not based on AutoRoute, a manual ModelFormAction instance needs
    -- to be defined
    modelFormAction record =
        let
            path = theRequest |> get #pathInfo
            action = if isNew record
                then "Create" <> getModelName @record
                else "Update" <> getModelName @record <> "?" <> lcfirst (getModelName @record) <> "Id=" <> tshow (get #id record)
        in
            init path
                |> (\path -> [""] <> (fromMaybe [] path) <> [action])
                |> intercalate "/"