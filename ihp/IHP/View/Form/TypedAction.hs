{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: IHP.View.Form.TypedAction
Description: Form helpers for typed GADT actions

This module connects existing IHP form rendering to typed routes. The form
action URL, method, and encoding come from the typed action route, while a
type-level check ensures the target action accepts a browser-compatible body
encoding.
-}
module IHP.View.Form.TypedAction
    ( FormSpec (..)
    , FormCompatibleBodyEncodings (..)
    , formSpec
    , formSpecField
    , formSpecTextField
    , formSpecNumberField
    , formSpecUrlField
    , formSpecTextareaField
    , formSpecColorField
    , formSpecEmailField
    , formSpecDateField
    , formSpecDateTimeField
    , formSpecPasswordField
    , formSpecHiddenField
    , formSpecCheckboxField
    , formSpecSelectField
    , formSpecRadioField
    , formSpecFileField
    , formSpecSubmitButton
    , formForAction
    , formForActionWithOptions
    ) where

import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Proxy (Proxy)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import IHP.Controller.Context (ControllerContext)
import IHP.Controller.TypedAction
import IHP.HSX.Markup (Markup, ToHtml (toHtml))
import IHP.HSX.MarkupQQ (hsx)
import IHP.ModelSupport (InputValue (..))
import IHP.NameSupport (fieldNameToFieldLabel)
import IHP.Router.TypedRoute
import IHP.Router.UrlGenerator (HasPath (..))
import IHP.View.Form.Fields (DateTimeValue (..))
import IHP.View.Form.FormFor (buildForm)
import IHP.ViewSupport (theCSSFramework)
import IHP.View.Types (FormContext (..), FormField (..), InputType (..), SubmitButton (..))
import Network.HTTP.Types.Method (StdMethod (..), renderStdMethod)
import Network.Wai (Request)
import Prelude

-- | A typed form body renderer for an input record.
--
-- The input type should normally be the same type used by the action body:
--
-- @
-- data ProjectsAction request response where
--     UpdateProjectAction :: Id Project -> ProjectsAction ('Body ProjectInput) EditView
--
-- projectForm :: FormSpec ProjectInput
-- projectForm = formSpec [hsx|
--     {(formSpecTextField #name) { fieldLabel = "Project name", required = True }}
--     {(formSpecCheckboxField #enabled) { fieldLabel = "Enabled?" }}
--     {formSpecSubmitButton "Save project"}
-- |]
-- @
newtype FormSpec input = FormSpec
    { renderFormSpec :: (?context :: ControllerContext, ?formContext :: FormContext input) => Markup
    }

instance Semigroup (FormSpec input) where
    FormSpec left <> FormSpec right = FormSpec (left <> right)
    {-# INLINE (<>) #-}

instance Monoid (FormSpec input) where
    mempty = FormSpec mempty
    {-# INLINE mempty #-}

-- | Builds a typed form spec from regular HSX markup.
formSpec :: ((?context :: ControllerContext, ?formContext :: FormContext input) => Markup) -> FormSpec input
formSpec = FormSpec
{-# INLINE formSpec #-}

formSpecFieldWithValue ::
    forall field input.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    ) =>
    Proxy field ->
    InputType ->
    Text ->
    FormField
formSpecFieldWithValue field fieldType fieldValue =
    FormField
        { fieldType
        , fieldName = ?formContext.fieldNamePrefix <> fieldName
        , fieldLabel = fieldNameToFieldLabel fieldName
        , fieldValue
        , fieldInputId = fieldName
        , validatorResult = Nothing
        , additionalAttributes = []
        , fieldClass = ""
        , labelClass = ""
        , disabled = False
        , disableLabel = False
        , disableGroup = False
        , disableValidationResult = False
        , cssFramework = ?formContext.cssFramework
        , helpText = ""
        , placeholder = ""
        , required = False
        , autofocus = False
        }
  where
    fieldName = cs (symbolVal field)
{-# INLINE formSpecFieldWithValue #-}

-- | Builds a customizable typed form field for input records that do not have
-- IHP model metadata. Use record update syntax to customize label, classes,
-- placeholder, required state, help text, or extra HTML attributes.
formSpecField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    InputType ->
    FormField
formSpecField field fieldType =
    formSpecFieldWithValue field fieldType (inputValue ((getField @field ?formContext.model) :: value))
{-# INLINE formSpecField #-}

formSpecTextField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecTextField field = formSpecField field TextInput
{-# INLINE formSpecTextField #-}

formSpecNumberField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecNumberField field = formSpecField field NumberInput
{-# INLINE formSpecNumberField #-}

formSpecUrlField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecUrlField field = formSpecField field UrlInput
{-# INLINE formSpecUrlField #-}

formSpecTextareaField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecTextareaField field = formSpecField field TextareaInput
{-# INLINE formSpecTextareaField #-}

formSpecColorField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecColorField field = formSpecField field ColorInput
{-# INLINE formSpecColorField #-}

formSpecEmailField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecEmailField field = formSpecField field EmailInput
{-# INLINE formSpecEmailField #-}

formSpecDateField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecDateField field = formSpecField field DateInput
{-# INLINE formSpecDateField #-}

formSpecDateTimeField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    , DateTimeValue value
    ) =>
    Proxy field ->
    FormField
formSpecDateTimeField field =
    (formSpecField field DateTimeInput)
        { fieldValue = dateTimeValue ((getField @field ?formContext.model) :: value)
        }
{-# INLINE formSpecDateTimeField #-}

formSpecPasswordField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecPasswordField field = formSpecField field PasswordInput
{-# INLINE formSpecPasswordField #-}

formSpecHiddenField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    FormField
formSpecHiddenField field =
    (formSpecField field HiddenInput)
        { disableLabel = True
        , disableGroup = True
        , disableValidationResult = True
        }
{-# INLINE formSpecHiddenField #-}

formSpecCheckboxField ::
    forall field input.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input Bool
    ) =>
    Proxy field ->
    FormField
formSpecCheckboxField field =
    (formSpecField field CheckboxInput)
        { fieldValue =
            if getField @field ?formContext.model
                then "yes"
                else "no"
        }
{-# INLINE formSpecCheckboxField #-}

formSpecSelectField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    [(Text, Text)] ->
    FormField
formSpecSelectField field options =
    formSpecField field (SelectInput options)
{-# INLINE formSpecSelectField #-}

formSpecRadioField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    , InputValue value
    ) =>
    Proxy field ->
    [(Text, Text)] ->
    FormField
formSpecRadioField field options =
    formSpecField field (RadioInput options)
{-# INLINE formSpecRadioField #-}

formSpecFileField ::
    forall field input value.
    ( ?formContext :: FormContext input
    , KnownSymbol field
    , HasField field input value
    ) =>
    Proxy field ->
    FormField
formSpecFileField field =
    formSpecFieldWithValue field FileInput ""
{-# INLINE formSpecFileField #-}

-- | Renders a submit button inside a 'FormSpec'.
formSpecSubmitButton :: (?formContext :: FormContext input) => Text -> SubmitButton
formSpecSubmitButton label =
    SubmitButton
        { label = toHtml label
        , buttonClass = mempty
        , buttonDisabled = False
        , cssFramework = ?formContext.cssFramework
        }
{-# INLINE formSpecSubmitButton #-}

-- | Renders a 'FormSpec' so it submits to a typed GADT action.
--
-- The route URL is taken from @pathTo typedAction@. The HTML form method is
-- chosen from the typed route methods. Non-GET/POST methods are submitted via
-- POST with IHP's existing @_method@ override field.
--
-- This helper only compiles when the action body accepts a form-compatible
-- encoding: @application/x-www-form-urlencoded@ or @multipart/form-data@.
-- JSON-only typed actions therefore cannot accidentally be targeted by a
-- browser form.
formForAction ::
    forall action body response input.
    ( ?context :: ControllerContext
    , ?request :: Request
    , HasPath (action body response)
    , HasActionMethods (action body response)
    , FormCompatibleBodyEncodings (BodyEncodings body)
    , DecodedRequest body ~ input
    ) =>
    action body response ->
    input ->
    FormSpec input ->
    Markup
formForAction typedAction input formSpec' =
    formForActionWithOptions typedAction input id formSpec'
{-# INLINE formForAction #-}

-- | Like 'formForAction', but allows changing the generated 'FormContext'
-- before rendering the form.
formForActionWithOptions ::
    forall action body response input.
    ( ?context :: ControllerContext
    , ?request :: Request
    , HasPath (action body response)
    , HasActionMethods (action body response)
    , FormCompatibleBodyEncodings (BodyEncodings body)
    , DecodedRequest body ~ input
    ) =>
    action body response ->
    input ->
    (FormContext input -> FormContext input) ->
    FormSpec input ->
    Markup
formForActionWithOptions typedAction input applyOptions FormSpec{renderFormSpec} =
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
            renderFormSpec
{-# INLINE formForActionWithOptions #-}

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
        }
{-# INLINE createInputFormContext #-}

-- | Selects the form-compatible body encoding accepted by a typed action.
--
-- If both URL-encoded forms and multipart forms are accepted, the first
-- encoding in the action's type-level list wins. JSON-only actions have no
-- instance, so they cannot be passed to 'formForAction'.
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
