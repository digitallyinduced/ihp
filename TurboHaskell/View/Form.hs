{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances, StandaloneDeriving,InstanceSigs, UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables, IncoherentInstances  #-}

module TurboHaskell.View.Form where

import           ClassyPrelude                      hiding (div)
import           Data.String.Conversions            (cs)
import           TurboHaskell.HaskellSupport
import qualified TurboHaskell.ModelSupport
import           TurboHaskell.ValidationSupport
import           TurboHaskell.View.ConvertibleStrings ()
import           TurboHaskell.ViewErrorMessages
import           TurboHaskell.ViewSupport
import           Network.HTTP.Types.Method          (methodPost)
import           Network.Wai                        (requestMethod)
import qualified Network.Wai
import           Text.Blaze                         (Attribute, dataAttribute, preEscapedText, stringValue, text)
import           Text.Blaze.Html5                   (a, body, button, code, div, docTypeHtml, footer, form, h1, h2, h3, h4, h5, h6, head, hr, html, iframe, img,
                                                     input, label, li, link, meta, nav, ol, p, pre, script, small, span, table, tbody, td, th, thead, title, tr,
                                                     ul)
import           Text.Blaze.Html5                   ((!))
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Html5                   as Html5
import           Text.Blaze.Html5.Attributes        (autocomplete, autofocus, charset, class_, content, href, httpEquiv, id, lang, method, name,
                                                     onclick, placeholder, rel, src, style, type_, value)
import qualified Text.Blaze.Html5.Attributes        as A

import qualified Text.Blaze.Internal
import TurboHaskell.HtmlSupport.ToHtml
import qualified TurboHaskell.Controller.Session
import qualified TurboHaskell.NameSupport
import GHC.OverloadedLabels
import GHC.Types
import GHC.TypeLits
import Data.Proxy
import qualified Data.Text
import qualified Text.Inflections
import GHC.Records
import qualified Data.Text as Text
import Control.Lens hiding ((|>), (:>))
import Data.Generics.Product hiding (HasField, getField)
import qualified Data.Generics.Product
import GHC.Generics
import Data.Default
import Data.Dynamic
import Data.Maybe (fromJust)
import TurboHaskell.Controller.RequestContext
import TurboHaskell.RouterSupport
import TurboHaskell.ModelSupport (getModelName, GetModelName, Id', FieldWithDefault, NormalizeModel)

import TurboHaskell.RouterSupport (RestfulController (..), RestfulControllerId, Child, HasPath, PathArgument, Child, Parent)
import GHC.Records


class ModelFormAction controllerContext formObject where
    modelFormAction :: formObject -> Text



instance ModelFormAction controllerContext (parent, child) where
    {-# INLINE modelFormAction #-}
    modelFormAction (parent, child) = undefined


-- modelFormAction (user :: User) or modelFormAction (newUser :: New User)
instance (
        HasField "id" formObject id
        , controller ~ ModelControllerMap controllerContext (NormalizeModel formObject)
        , Child controller ~ controller
        , HasPath controller
        , RestfulController controller
        , ModelFormActionTopLevelResource controller id
        , FrontControllerPrefix (ControllerApplicationMap controller)
        ) => ModelFormAction controllerContext formObject where
    {-# INLINE modelFormAction #-}
    modelFormAction formObject = modelFormActionTopLevelResource (Proxy @controller) (getField @"id" formObject)


class ModelFormActionTopLevelResource controller id where
    modelFormActionTopLevelResource :: Proxy controller -> id -> Text

instance (
        Child controller ~ controller
        , HasPath controller
        , RestfulController controller
        , FrontControllerPrefix (ControllerApplicationMap controller)
        ) => ModelFormActionTopLevelResource controller (FieldWithDefault id') where
    {-# INLINE modelFormActionTopLevelResource #-}
    modelFormActionTopLevelResource _ _ = pathTo (fromJust (createAction @controller))

instance (
        RestfulControllerId controller ~ Id' table
        , Child controller ~ controller
        , HasPath controller
        , RestfulController controller
        , HasPath controller
        , FrontControllerPrefix (ControllerApplicationMap controller)
        ) => ModelFormActionTopLevelResource controller (Id' (table :: Symbol)) where
    {-# INLINE modelFormActionTopLevelResource #-}
    modelFormActionTopLevelResource _ id = pathTo ((fromJust (updateAction @controller)) id)


-- modelFormAction (newUser :: New User)




--instance {-# OVERLAPPABLE #-} TypeError (GHC.TypeLits.Text "formfor could not find a ModelFormAction instance for your form. Please write one manually, e.g. " :$$: GHC.TypeLits.Text "instance ModelFormAction Project where" :$$: GHC.TypeLits.Text "    modelFormAction = \"/Projects\" ")
--    => ModelFormAction catchAll where
--    modelFormAction = undefined

data FormField = FormField {
        fieldType :: !InputType,
        fieldName :: !Html5.AttributeValue,
        fieldLabel :: !Text,
        fieldValue :: !Text,
        fieldInputId :: !Text,
        validatorResult :: !ValidatorResult,
        fieldInput :: !(FormField -> Html5.Html),
        fieldClass :: !Html5.AttributeValue,
        labelClass :: !Html5.AttributeValue,
        disableLabel :: !Bool,
        disableGroup :: !Bool,
        disableValidationResult :: !Bool,
        modelIsNew :: !Bool,
        formIsSubmitted :: !Bool,
        renderFormField :: FormField -> Html5.Html,
        helpText :: !Text,
        placeholder :: !Text
    }

data SubmitButton = SubmitButton { modelIsNew :: !Bool, modelName :: !Text, renderSubmit :: SubmitButton -> Html5.Html, label :: Html5.Html, buttonClass :: Text }

data FormContext model =
    FormContext
        { model :: model
        , validatorResult :: ValidatorResultFor model
        , renderFormField :: FormField -> Html5.Html
        , renderSubmit :: SubmitButton -> Html5.Html
        , request :: Network.Wai.Request
        , formAction :: !Text
        }

{-# INLINE formFor #-}
formFor :: forall model viewContext parent id formObject controllerContext. (
        ?viewContext :: viewContext
        , HasField "validations" viewContext [Dynamic]
        , HasField "requestContext" viewContext RequestContext
        , Eq model
        , Typeable model
        , Typeable (ValidatorResultFor model)
        , Default (ValidatorResultFor model)
        , ModelFormAction controllerContext formObject
        , HasField "id" model id
        , TurboHaskell.ModelSupport.IsNewId id
        , FormObject formObject
        , model ~ FormObjectModel formObject
        , HasPath (ModelControllerMap controllerContext (NormalizeFormObject formObject))
        , controllerContext ~ ControllerContext viewContext
        ) => formObject -> ((?viewContext :: viewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor formObject = formFor' (createFormContext formObject)


{-# INLINE horizontalFormFor #-}
horizontalFormFor :: forall model viewContext parent id formObject controllerContext. (
        ?viewContext :: viewContext
        , HasField "validations" viewContext [Dynamic]
        , HasField "requestContext" viewContext RequestContext
        , Eq model
        , Typeable model
        , Typeable (ValidatorResultFor model)
        , Default (ValidatorResultFor model)
        , ModelFormAction controllerContext formObject
        , HasField "id" model id
        , TurboHaskell.ModelSupport.IsNewId id
        , FormObject formObject
        , model ~ FormObjectModel formObject
        , HasPath (Child (ModelControllerMap controllerContext formObject))
        , controllerContext ~ ControllerContext viewContext
        ) => formObject -> ((?viewContext :: viewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
horizontalFormFor formObject = formFor' (createFormContext formObject)
        { renderFormField = renderHorizontalBootstrapFormField
        , renderSubmit = renderHorizontalBootstrapSubmitButton
        }

{-# INLINE createFormContext #-}
createFormContext :: forall model viewContext parent id formObject controllerContext. (
        ?viewContext :: viewContext
        , HasField "validations" viewContext [Dynamic]
        , HasField "requestContext" viewContext RequestContext
        , Eq model
        , Typeable model
        , Typeable (ValidatorResultFor model)
        , Default (ValidatorResultFor model)
        , ModelFormAction controllerContext formObject
        , HasField "id" model id
        , TurboHaskell.ModelSupport.IsNewId id
        , FormObject formObject
        , model ~ FormObjectModel formObject
        , controllerContext ~ ControllerContext viewContext
        ) => formObject -> FormContext model
createFormContext formObject = 
    FormContext
        { model
        , renderFormField = renderBootstrapFormField
        , renderSubmit = renderBootstrapSubmitButton
        , validatorResult = findValidatorResult ?viewContext model
        , request = getField @"request" (getField @"requestContext" ?viewContext)
        , formAction = (modelFormAction @controllerContext) formObject
        }
            where
                model = getModel formObject

-- We have to deal with different kind of models passed into the formFor helper.
-- E.g.
-- formFor user
-- formFor (user, post)
-- formFor (user, post, comment)
class FormObject object where
    getModel :: object -> FormObjectModel object

type family FormObjectModel object where
    FormObjectModel (parent, model) = model
    FormObjectModel model = model

type family NormalizeFormObject object where
    NormalizeFormObject (a, b) = (NormalizeModel a, NormalizeModel b)
    NormalizeFormObject a = NormalizeModel a

instance FormObject (parent, model) where
    getModel (_, model) = model

instance (FormObjectModel model ~ model) => FormObject model where
    getModel model = model

{-# INLINE findValidatorResult #-}
findValidatorResult :: forall model viewContext. (Typeable model, Typeable (ValidatorResultFor model), Default (ValidatorResultFor model), Eq model, ?viewContext :: viewContext, HasField "validations" viewContext [Dynamic]) => viewContext -> model -> ValidatorResultFor model
findValidatorResult viewContext model =
    let
        validations = getField @"validations" viewContext
        isValidationForModel :: Dynamic -> Bool
        isValidationForModel dyn =
            case (fromDynamic dyn) :: Maybe (model, ValidatorResultFor model) of
                Nothing -> False
                Just (model', errors) -> model' == model
    in
        maybe def (snd . fromJust . (fromDynamic @(model, ValidatorResultFor model) )) (find isValidationForModel validations)



{-# INLINE formFor' #-}
formFor' :: forall model viewContext parent id. (?viewContext :: viewContext, HasField "validations" viewContext [Dynamic]) => (HasField "id" model id, TurboHaskell.ModelSupport.IsNewId id) => FormContext model -> ((?viewContext :: viewContext, ?formContext :: FormContext model) => Html5.Html) -> Html5.Html
formFor' formContext inner =
    let
        theModel = model formContext
        action = cs $ (formAction formContext)
        isNewRecord = TurboHaskell.ModelSupport.isNew theModel
        formId = if isNewRecord then "" else cs (formAction formContext)
        formClass = if isNewRecord then "new-form" else "edit-form"
    in
        form ! A.method "POST" ! A.action action ! A.id formId ! A.class_ formClass $ do
            let ?formContext = formContext in inner

{-# INLINE submitButton #-}
submitButton :: forall model id. (?formContext :: FormContext model, HasField "id" model id, TurboHaskell.ModelSupport.IsNewId id, KnownSymbol (GetModelName model)) => SubmitButton
submitButton =
    let
        modelName = TurboHaskell.ModelSupport.getModelName @model
        isNew = TurboHaskell.ModelSupport.isNew (model ?formContext)
    in SubmitButton
    { modelIsNew = isNew
    , modelName = modelName
    , renderSubmit = let FormContext { renderSubmit } = ?formContext in renderSubmit
    , label = cs $ (if isNew then "Create " else "Save ") <> modelName
    , buttonClass = mempty
    }

data InputType = TextInput | CheckboxInput | ColorInput | EmailInput | HiddenInput | TextareaInput | DateInput | DateTimeInput | PasswordInput | SelectInput { options :: [(Text, Text)] }

{-# INLINE renderHelpText #-}
renderHelpText (FormField { helpText }) =
    case helpText of
        "" -> mempty
        helpText -> small ! A.class_ "form-text text-muted" $ text helpText

{-# INLINE renderValidationResult #-}
renderValidationResult (FormField { modelIsNew, validatorResult }) = case validatorResult of
                Success         -> return ()
                Failure message -> div ! class_ "invalid-feedback" $ cs message

{-# INLINE isInvalid #-}
isInvalid :: FormField -> Bool
isInvalid (FormField { modelIsNew, formIsSubmitted, validatorResult }) =
        if formIsSubmitted
            then case validatorResult of
                    Success         -> False
                    Failure _       -> True
            else False

{-# INLINE renderBootstrapFormField #-}
renderBootstrapFormField :: FormField -> Html5.Html
renderBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            PasswordInput -> renderTextField "password" formField
            ColorInput -> renderTextField "color" formField
            EmailInput -> renderTextField "email" formField
            DateInput -> renderTextField "date" formField
            DateTimeInput -> renderTextField "datetime" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
            TextareaInput -> renderTextField "text" formField
            SelectInput {} -> renderSelectField formField
    where
        maybeWithFormGroup (FormField { fieldInputId, disableGroup }) renderInner = if disableGroup then renderInner else div ! A.class_ "form-group" ! A.id (cs $ "form-group-" <> fieldInputId) $ renderInner
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else H.label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                Html5.text fieldLabel
                if disableValidationResult then mempty else renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass, placeholder }) =
            maybeWithFormGroup formField $ do
                if disableLabel || fieldLabel == "" then return () else H.label ! A.class_ labelClass ! A.for (cs fieldInputId) $ cs fieldLabel
                let theInput = (fieldInput formField) ! type_ inputType ! name fieldName ! A.placeholder (cs placeholder) ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "" then theInput else theInput ! value (cs fieldValue)
                if disableValidationResult then mempty else renderValidationResult formField
                renderHelpText formField
        renderSelectField :: FormField -> Html5.Html
        renderSelectField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) =
            maybeWithFormGroup formField $ do
                if disableLabel then return () else H.label ! A.class_ labelClass ! A.for (cs fieldInputId) $ cs fieldLabel
                Html5.select ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue) $ do
                    --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                    let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                    (if isValueSelected then Html5.option else Html5.option ! A.selected "selected")  ! A.disabled "disabled" $ Html5.text ("Please select")
                    forM_ (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                renderHelpText formField
                if disableValidationResult then mempty else renderValidationResult formField

{-# INLINE renderBootstrapSubmitButton #-}
renderBootstrapSubmitButton SubmitButton { modelIsNew, modelName, label, buttonClass }= button ! class_ (cs $ "btn btn-primary" <> (if buttonClass /= mempty then " " <> buttonClass else "")) $ label


{-# INLINE renderHorizontalBootstrapFormField #-}
renderHorizontalBootstrapFormField :: FormField -> Html5.Html
renderHorizontalBootstrapFormField formField@(FormField { fieldType }) =
        case fieldType of
            TextInput -> renderTextField "text" formField
            ColorInput -> renderTextField "color" formField
            EmailInput -> renderTextField "email" formField
            DateInput -> renderTextField "date" formField
            CheckboxInput -> renderCheckboxFormField formField
            HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
            TextareaInput -> renderTextField "text" formField
            SelectInput {} -> renderSelectField formField
    where
        renderCheckboxFormField :: FormField -> Html5.Html
        renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted }) = div ! class_ "form-group" $ div ! class_ "form-check" $ do
            (if disableLabel then div else H.label ! class_ "form-check-label") $ do
                let theInput = input ! type_ "checkbox" ! name fieldName ! class_ ("form-check-input " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass)
                if fieldValue == "yes" then theInput ! A.checked "checked" else theInput
                Html5.text fieldLabel
                if disableValidationResult then mempty else renderValidationResult formField
        renderTextField :: Html5.AttributeValue -> FormField -> Html5.Html
        renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass, placeholder }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
            where
                renderInner = do
                    if disableLabel || fieldLabel == "" then return () else H.label ! A.class_ ("col-sm-4 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ cs fieldLabel
                    div ! class_ "col-sm-8" $ do
                        (fieldInput formField) ! type_ inputType ! name fieldName ! A.placeholder (cs placeholder) ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue)
                        if disableValidationResult then mempty else renderValidationResult formField
                        renderHelpText formField
        renderSelectField :: FormField -> Html5.Html
        renderSelectField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, modelIsNew, formIsSubmitted, labelClass }) = if disableLabel then renderInner else div ! A.class_ "form-group row" $ renderInner
            where
                renderInner = do
                    if disableLabel || fieldLabel == "" then return () else H.label ! A.class_ ("col-sm-4 col-form-label " <> labelClass) ! A.for (cs fieldInputId) $ cs fieldLabel
                    div ! class_ "col-sm-8" $ do
                        Html5.select ! name fieldName ! A.id (cs fieldInputId) ! class_ ("form-control " <> (if not formIsSubmitted || isSuccess validatorResult then "" else "is-invalid") <> " " <> fieldClass) ! value (cs fieldValue) $ do
                            --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                            let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                            (if isValueSelected then Html5.option else Html5.option ! A.selected "selected") ! A.disabled "disabled" $ Html5.text ("Please select")
                            forM_ (options fieldType) $ \(optionLabel, optionValue) -> (let option = Html5.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                        if disableValidationResult then mempty else renderValidationResult formField
                        renderHelpText formField

{-# INLINE renderHorizontalBootstrapSubmitButton #-}
renderHorizontalBootstrapSubmitButton SubmitButton { modelIsNew, modelName }= div ! class_ "form-group row" $ do
    div ! class_ "offset-sm-5 col-sm-3 text-left" $ do
        button ! class_ "btn btn-primary btn-lg pl-4 pr-4 w-100" $ (if modelIsNew then "Create " else "Save ") <> (cs $ modelName)


data TextFieldTag


instance (
        KnownSymbol symbol
        , HasField "id" model id, TurboHaskell.ModelSupport.IsNewId id
        --, TurboHaskell.ModelSupport.HasModelName model
        , HasField symbol model value
        , HasField symbol (TurboHaskell.ModelSupport.ColumnNamesRecord model) ByteString
        , TurboHaskell.ModelSupport.ColumnNames model
        , TurboHaskell.ModelSupport.InputValue value
        , (HasField' symbol (ValidatorResultFor model) ValidatorResult)
        , (Data.Generics.Product.HasField' symbol (ValidatorResultFor model) ValidatorResult)
        , (Generic (ValidatorResultFor model))
        , KnownSymbol (GetModelName model)
    ) => IsLabel symbol ((FormContext model, Proxy TextFieldTag) -> FormField) where
    {-# INLINE fromLabel #-}
    fromLabel = \(formContext, _) -> let columnName = (cs $ getField @symbol (TurboHaskell.ModelSupport.columnNames (Proxy @model))) in FormField {
                        fieldType = TextInput,
                        fieldName = cs columnName,
                        fieldLabel = columnNameToFieldLabel columnName,
                        fieldValue =  let value :: value = getField @(symbol) (model formContext) in TurboHaskell.ModelSupport.inputValue value,
                        fieldInputId = cs (TurboHaskell.NameSupport.lcfirst (getModelName @model) <> "_" <> columnName),
                        validatorResult = (Data.Generics.Product.getField @symbol (let FormContext { validatorResult } = formContext in validatorResult)) :: ValidatorResult,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = const input,
                        modelIsNew = TurboHaskell.ModelSupport.isNew (model formContext),
                        formIsSubmitted = isSubmitted' formContext,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

instance (
        KnownSymbol symbol
        , HasField "id" model id, TurboHaskell.ModelSupport.IsNewId id
        , HasField symbol model Bool
        , HasField symbol (TurboHaskell.ModelSupport.ColumnNamesRecord model) ByteString
        , TurboHaskell.ModelSupport.ColumnNames model
        , KnownSymbol (GetModelName model)
    ) => IsLabel symbol ((FormContext model, Proxy Bool) -> FormField) where
    {-# INLINE fromLabel #-}
    fromLabel = \(formContext, _) -> let columnName = (cs $ getField @symbol (TurboHaskell.ModelSupport.columnNames (Proxy @model))) in FormField {
                        fieldType = CheckboxInput,
                        fieldName = cs columnName,
                        fieldLabel = columnNameToFieldLabel columnName,
                        fieldValue =  let value = getField @(symbol) (model formContext) in if value then "yes" else "no",
                        fieldInputId = cs (TurboHaskell.NameSupport.lcfirst (getModelName @model) <> "_" <> columnName),
                        validatorResult = Success,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = const input,
                        modelIsNew = TurboHaskell.ModelSupport.isNew (model formContext),
                        formIsSubmitted = isSubmitted' formContext,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

instance (
        KnownSymbol symbol
        , HasField "id" model id, TurboHaskell.ModelSupport.IsNewId id
        , HasField symbol model ((SelectValue item))
        , CanSelect item
        , TurboHaskell.ModelSupport.InputValue (SelectValue item)
        , HasField symbol (TurboHaskell.ModelSupport.ColumnNamesRecord model) ByteString
        , TurboHaskell.ModelSupport.ColumnNames model
        , (Data.Generics.Product.HasField' symbol (ValidatorResultFor model) ValidatorResult)
        , (Generic (ValidatorResultFor model))
        , (KnownSymbol (GetModelName model))
    ) => IsLabel symbol ((FormContext model, [item], Proxy value) -> FormField) where
    {-# INLINE fromLabel #-}
    fromLabel = \(formContext, items, _) -> let columnName = (cs $ getField @symbol (TurboHaskell.ModelSupport.columnNames (Proxy @model))) in FormField {
                        fieldType =
                            let
                                itemToTuple :: item -> (Text, Text)
                                itemToTuple item = (selectLabel item, TurboHaskell.ModelSupport.inputValue (selectValue item))
                            in
                                 SelectInput $ map itemToTuple items
                            ,
                        fieldName = cs columnName,
                        fieldLabel = removeIdSuffix $ columnNameToFieldLabel columnName,
                        fieldValue =
                            let value = ((getField @(symbol) (model formContext)) :: (SelectValue item))
                            in TurboHaskell.ModelSupport.inputValue value,
                        fieldInputId = cs (TurboHaskell.NameSupport.lcfirst (getModelName @model) <> "_" <> columnName),
                        validatorResult = (Data.Generics.Product.getField @symbol (let FormContext { validatorResult } = formContext in validatorResult)) :: ValidatorResult,
                        fieldClass = "",
                        labelClass = "",
                        disableLabel = False,
                        disableGroup = False,
                        disableValidationResult = False,
                        fieldInput = const (Html5.select mempty),
                        modelIsNew = TurboHaskell.ModelSupport.isNew (model formContext),
                        formIsSubmitted = isSubmitted' formContext,
                        renderFormField = let FormContext { renderFormField } = formContext in renderFormField,
                        helpText = "",
                        placeholder = ""
                    }

{-# INLINE fieldNameToFieldLabel #-}
fieldNameToFieldLabel :: Text -> Text
fieldNameToFieldLabel fieldName = cs (let (Right parts) = Text.Inflections.parseCamelCase [] fieldName in Text.Inflections.titleize parts)

{-# INLINE columnNameToFieldLabel #-}
columnNameToFieldLabel :: Text -> Text
columnNameToFieldLabel columnName = cs (let (Right parts) = Text.Inflections.parseSnakeCase [] columnName in Text.Inflections.titleize parts)

{-# INLINE removeIdSuffix #-}
removeIdSuffix :: Text -> Text
removeIdSuffix text = fromMaybe text (Text.stripSuffix " Id" text)

{-# INLINE textField #-}
textField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
textField alpha = alpha (?formContext, Proxy :: Proxy TextFieldTag)

{-# INLINE textareaField #-}
textareaField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
textareaField alpha = (textField alpha) { fieldType = TextareaInput, fieldInput = \formField -> Html5.textarea (cs $ fieldValue formField) }

{-# INLINE colorField #-}
colorField :: forall alpha attributeName model. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
colorField alpha = (textField alpha) { fieldType = ColorInput }

{-# INLINE emailField #-}
emailField :: forall alpha attributeName model. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
emailField alpha = (textField alpha) { fieldType = EmailInput }

{-# INLINE dateField #-}
dateField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
dateField alpha = (textField alpha) { fieldType = DateInput }

{-# INLINE passwordField #-}
passwordField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
passwordField alpha = (textField alpha) { fieldType = PasswordInput }

{-# INLINE dateTimeField #-}
dateTimeField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
dateTimeField alpha = (textField alpha) { fieldType = DateTimeInput }

{-# INLINE hiddenField #-}
hiddenField :: forall alpha attributeName model value. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy TextFieldTag) -> FormField)) => alpha -> FormField
hiddenField alpha = (textField alpha) { fieldType = HiddenInput }

{-# INLINE checkboxField #-}
checkboxField :: forall alpha attributeName model. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, Proxy Bool) -> FormField)) => alpha -> FormField
checkboxField alpha = alpha (?formContext, Proxy :: Proxy Bool)

{-# INLINE selectField #-}
selectField :: forall alpha attributeName model value item. (?formContext :: FormContext model) => (alpha ~ ((FormContext model, [item], Proxy value) -> FormField), CanSelect item) => alpha -> [item] -> FormField
selectField alpha items = alpha (?formContext, items, Proxy :: Proxy value)

class CanSelect model where
    type SelectValue model :: GHC.Types.*
    selectLabel :: model -> Text
    selectValue :: model -> SelectValue model

instance ToHtml FormField where
    {-# INLINE toHtml #-}
    toHtml ::  FormField -> Html5.Html
    toHtml formField@(FormField { renderFormField }) = renderFormField formField

instance ToHtml SubmitButton where
    {-# INLINE toHtml #-}
    toHtml submitButton@(SubmitButton { renderSubmit }) = renderSubmit submitButton

renderFlashMessages :: forall viewContext. (?viewContext :: viewContext, HasField "flashMessages" viewContext [TurboHaskell.Controller.Session.FlashMessage]) => Html5.Html
renderFlashMessages =
    let flashMessages = (getField @"flashMessages" ?viewContext) :: [TurboHaskell.Controller.Session.FlashMessage]
    in
        forM_ flashMessages $ \flashMessage -> do
            case flashMessage of
                TurboHaskell.Controller.Session.SuccessFlashMessage message -> div ! class_ "alert alert-success" $ cs message
                TurboHaskell.Controller.Session.ErrorFlashMessage message -> div ! class_ "alert alert-danger" $ cs message

{-# INLINE isSubmitted #-}
isSubmitted :: (?viewContext :: viewContext, HasField "requestContext" viewContext RequestContext) => Bool
isSubmitted = let request = getField @"request" (getField @"requestContext" ?viewContext) in requestMethod request == methodPost

isSubmitted' :: FormContext model -> Bool
isSubmitted' formContext = let request = getField @"request" formContext in requestMethod request == methodPost
