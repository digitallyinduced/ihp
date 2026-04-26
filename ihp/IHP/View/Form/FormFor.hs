{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: IHP.View.Form.FormFor
Description: 'formFor' and form building utilities
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.Form.FormFor where

import           IHP.Controller.Context
import           IHP.Controller.TypedAction
import           IHP.HSX.ConvertibleStrings ()
import           IHP.HSX.MarkupQQ (hsx)
import           IHP.ModelSupport (Id', InputValue, getModelName, isNew)
import           IHP.Prelude
import           IHP.Router.TypedRoute
import           IHP.Router.UrlGenerator (HasPath (..))
import           IHP.ValidationSupport.Types (getValidationViolation)
import           IHP.View.Form.Fields (hiddenField)
import           IHP.View.Types
import           IHP.ViewSupport
import           Network.HTTP.Types.Method (StdMethod (..), renderStdMethod)
import           Network.Wai (Request, pathInfo)
import IHP.HSX.Markup (Markup, ToHtml(..))

type FormMarkup model = (?context :: ControllerContext, ?formContext :: FormContext model) => Markup

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
    , ?request :: Request
    , ModelFormAction record
    , HasField "meta" record MetaBag
    , KnownSymbol (GetModelName record)
    ) => record -> FormMarkup record -> Markup
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
    , ?request :: Request
    , ModelFormAction record
    , HasField "meta" record MetaBag
    , KnownSymbol (GetModelName record)
    ) => record -> (FormContext record -> FormContext record) -> FormMarkup record -> Markup
formForWithOptions record applyOptions formBody =
    buildForm (applyOptions (createFormContext record) { formAction = modelFormAction record }) formBody
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
    , ?request :: Request
    , ModelFormAction record
    , HasField "meta" record MetaBag
    , KnownSymbol (GetModelName record)
    ) => record -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Markup) -> Markup
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
    , ?request :: Request
    , HasField "meta" record MetaBag
    , KnownSymbol (GetModelName record)
    ) => record -> Text -> ((?context :: ControllerContext, ?formContext :: FormContext record) => Markup) -> Markup
formFor' record action = buildForm (createFormContext record) { formAction = action }
{-# INLINE formFor' #-}

-- | Used by 'formFor' to make a new form context
createFormContext :: forall record. (
        ?request :: Request
        , HasField "meta" record MetaBag
        , KnownSymbol (GetModelName record)
        ) => record -> FormContext record
createFormContext record =
    FormContext
        { model = record
        , formAction = ""
        , formMethod = "POST"
        , formEnctype = Nothing
        , cssFramework = theCSSFramework
        , formId = ""
        , formClass = if isNew record then "new-form" else "edit-form"
        , customFormAttributes = []
        , disableJavascriptSubmission = False
        , fieldNamePrefix = ""
        , formFieldInputId = modelFieldInputId @record
        , formFieldValidationResult = \field -> getValidationViolation field record
        , formIsNew = isNew record
        , formSubmitLabel = modelSubmitLabel @record record
        }
{-# INLINE createFormContext #-}

modelFieldInputId :: forall record fieldName. (KnownSymbol (GetModelName record), KnownSymbol fieldName) => Proxy fieldName -> Text
modelFieldInputId field =
    cs (lcfirst (getModelName @record) <> "_" <> cs (symbolVal field))
{-# INLINE modelFieldInputId #-}

modelSubmitLabel :: forall record. (HasField "meta" record MetaBag, KnownSymbol (GetModelName record)) => record -> Markup
modelSubmitLabel record =
    toHtml $ (if isNew record then "Create " else "Save ") <> buttonText
  where
    modelName = getModelName @record
    buttonText = modelName |> humanize
{-# INLINE modelSubmitLabel #-}

createInputFormContext :: (?request :: Request) => input -> FormContext input
createInputFormContext input =
    FormContext
        { model = input
        , formAction = ""
        , formMethod = "POST"
        , formEnctype = Nothing
        , cssFramework = theCSSFramework
        , formId = ""
        , formClass = "typed-form"
        , customFormAttributes = []
        , disableJavascriptSubmission = False
        , fieldNamePrefix = ""
        , formFieldInputId = \field -> cs (symbolVal field)
        , formFieldValidationResult = \_ -> Nothing
        , formIsNew = False
        , formSubmitLabel = toHtml ("Submit" :: Text)
        }
{-# INLINE createInputFormContext #-}

formForAction ::
    forall action body response.
    ( ?context :: ControllerContext
    , ?request :: Request
    , HasPath (action body response)
    , HasActionMethods (action body response)
    , FormCompatibleBodyEncodings (BodyEncodings body)
    ) =>
    action body response ->
    DecodedRequest body ->
    FormMarkup (DecodedRequest body) ->
    Markup
formForAction typedAction input formBody =
    formForActionWithOptions typedAction input (\c -> c) formBody
{-# INLINE formForAction #-}

formForActionWithOptions ::
    forall action body response.
    ( ?context :: ControllerContext
    , ?request :: Request
    , HasPath (action body response)
    , HasActionMethods (action body response)
    , FormCompatibleBodyEncodings (BodyEncodings body)
    ) =>
    action body response ->
    DecodedRequest body ->
    (FormContext (DecodedRequest body) -> FormContext (DecodedRequest body)) ->
    FormMarkup (DecodedRequest body) ->
    Markup
formForActionWithOptions typedAction input applyOptions formBody =
    let routeMethod = typedActionFormMethod typedAction
        bodyEncoding = formBodyEncoding @(BodyEncodings body)
        formContext =
            applyOptions (createInputFormContext input)
                { formAction = pathTo typedAction
                , formMethod = htmlFormMethod routeMethod
                , formEnctype = Just (encodingMediaType bodyEncoding)
                }
     in buildForm formContext do
            methodOverrideField routeMethod
            formBody
{-# INLINE formForActionWithOptions #-}

-- | Used by 'formFor' to render the form
buildForm :: forall model. (?context :: ControllerContext) => FormContext model -> ((?context :: ControllerContext, ?formContext :: FormContext model) => Markup) -> Markup
buildForm formContext inner = [hsx|
        <form
            method={formContext.formMethod}
            action={formContext.formAction}
            enctype={formContext.formEnctype}
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
    ) => Proxy fieldName -> ((?context :: ControllerContext, ?formContext :: FormContext childRecord) => Markup) -> Markup
nestedFormFor field nestedRenderForm = forEach children renderChild
    where
        parentFormContext :: FormContext parentRecord
        parentFormContext = ?formContext

        renderChild :: childRecord -> Markup
        renderChild record = let ?formContext = buildNestedFormContext record in [hsx|
            {hiddenField #id}
            {nestedRenderForm}
        |]

        buildNestedFormContext :: childRecord -> FormContext childRecord
        buildNestedFormContext record =
            parentFormContext
                { model = record
                , fieldNamePrefix = symbolToText @fieldName <> "_"
                , formFieldInputId = modelFieldInputId @childRecord
                , formFieldValidationResult = \field -> getValidationViolation field record
                , formIsNew = isNew record
                , formSubmitLabel = modelSubmitLabel @childRecord record
                }

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
submitButton :: forall model. (?formContext :: FormContext model) => SubmitButton
submitButton =
    SubmitButton
    { label = ?formContext.formSubmitLabel
    , buttonClass = mempty
    , buttonDisabled = False
    , cssFramework = ?formContext.cssFramework
    }
{-# INLINE submitButton #-}

-- | Selects the form-compatible body encoding accepted by a typed action.
--
-- If both URL-encoded forms and multipart forms are accepted, the first
-- encoding in the action's type-level list wins. JSON-only actions have no
-- instance, so they cannot be targeted by 'formForAction'.
class FormCompatibleBodyEncodings (encodings :: [BodyEncoding]) where
    formBodyEncoding :: BodyEncoding

instance {-# OVERLAPPING #-} FormCompatibleBodyEncodings ('FormUrlEncoded ': rest) where
    formBodyEncoding = FormUrlEncoded
    {-# INLINE formBodyEncoding #-}

instance {-# OVERLAPPING #-} FormCompatibleBodyEncodings ('Multipart ': rest) where
    formBodyEncoding = Multipart
    {-# INLINE formBodyEncoding #-}

instance {-# OVERLAPPABLE #-} (FormCompatibleBodyEncodings rest) => FormCompatibleBodyEncodings (other ': rest) where
    formBodyEncoding = formBodyEncoding @rest
    {-# INLINE formBodyEncoding #-}

typedActionFormMethod ::
    forall action body response.
    (HasActionMethods (action body response)) =>
    action body response ->
    StdMethod
typedActionFormMethod typedAction =
    case typedActionMethods typedAction of
        Just methods -> preferredFormRouteMethod methods
        Nothing -> error "formForAction: no typed route matched this action"
{-# INLINE typedActionFormMethod #-}

preferredFormRouteMethod :: [StdMethod] -> StdMethod
preferredFormRouteMethod methods
    | POST `elem` methods = POST
    | GET `elem` methods = GET
    | HEAD `elem` methods = GET
    | otherwise =
        case methods of
            method : _ -> method
            [] -> error "formForAction: typed route has no allowed methods"
{-# INLINE preferredFormRouteMethod #-}

htmlFormMethod :: StdMethod -> Text
htmlFormMethod GET = "GET"
htmlFormMethod HEAD = "GET"
htmlFormMethod _ = "POST"
{-# INLINE htmlFormMethod #-}

methodOverrideField :: StdMethod -> Markup
methodOverrideField GET = mempty
methodOverrideField HEAD = mempty
methodOverrideField POST = mempty
methodOverrideField method = [hsx|<input type="hidden" name="_method" value={methodName}/>|]
  where
    methodName :: Text
    methodName = cs (renderStdMethod method)
{-# INLINE methodOverrideField #-}

-- | Returns the form's action attribute for a given record.
class ModelFormAction record where
    modelFormAction :: (?context :: ControllerContext, ?request :: Request) => record -> Text

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
