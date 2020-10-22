{-|
Module: IHP.View.CSSFramework
Description: Adds support for bootstrap, tailwind, etc. to IHP
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import qualified Text.Blaze.Html5 as Blaze
import IHP.HtmlSupport.QQ (hsx)
import IHP.HtmlSupport.ToHtml
import IHP.View.Types
import IHP.View.Classes

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), (!?))
import qualified Text.Blaze.Html5.Attributes as A
import IHP.ModelSupport

-- | Provides an unstyled CSSFramework
--
-- This way we can later add more properties to the CSSFramework without having update all the CSS Frameworks manually
instance Default CSSFramework where
    def = CSSFramework { .. }
        where
            styledFlashMessage _ (Session.SuccessFlashMessage message) = [hsx|<div>{message}</div>|]
            styledFlashMessage _ (Session.ErrorFlashMessage message) = [hsx|<div>{message}</div>|]

            styledFlashMessages cssFramework flashMessages = forEach flashMessages (styledFlashMessage cssFramework)
            
            styledFormField :: CSSFramework -> FormField -> Blaze.Html
            styledFormField cssFramework formField =
                    case get #fieldType formField of
                        TextInput -> renderTextField "text" formField
                        NumberInput -> renderTextField "number" formField
                        PasswordInput -> renderTextField "password" formField
                        ColorInput -> renderTextField "color" formField
                        EmailInput -> renderTextField "email" formField
                        DateInput -> renderTextField "date" formField
                        DateTimeInput -> renderTextField "datetime-local" formField
                        CheckboxInput -> renderCheckboxFormField formField
                        HiddenInput -> renderTextField "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True }
                        TextareaInput -> renderTextField "text" formField
                        SelectInput {} -> renderSelectField formField
                where
                    -- | Wraps the input inside a @<div class="form-group">...</div>@ (unless @disableGroup = True@)
                    formGroup :: Blaze.Html -> Blaze.Html
                    formGroup renderInner = case formField of
                        FormField { disableGroup = True } -> renderInner
                        FormField { fieldInputId } -> [hsx|<div class={formGroupClass} id={"form-group-" <> fieldInputId}>{renderInner}</div>|]

                    inputClass :: (Text, Bool)
                    inputClass = ((get #styledInputClass cssFramework) formField, True)

                    inputInvalidClass :: Text
                    inputInvalidClass = (get #styledInputInvalidClass cssFramework) formField

                    formGroupClass :: Text
                    formGroupClass = get #styledFormGroupClass cssFramework

                    helpText :: Blaze.Html
                    helpText = (get #styledFormFieldHelp cssFramework) cssFramework formField

                    validationResult :: Blaze.Html
                    validationResult = unless (get #disableValidationResult formField) ((get #styledValidationResult cssFramework) cssFramework formField)

                    renderCheckboxFormField :: FormField -> Blaze.Html
                    renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, labelClass, required }) = do
                        formGroup do
                            (H.div ! A.class_ "form-check") do
                                let element = if disableLabel then H.div else H.label ! A.class_ "form-check-label"
                                element do
                                    let theInput = H.input
                                            ! A.type_ "checkbox"
                                            ! A.name fieldName
                                            ! A.class_ (cs $ classes ["form-check-input", (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))])
                                            !? (required, A.required "required")
                                            !? (fieldValue == "yes", A.checked "checked")
                                    theInput
                                    H.input ! A.type_ "hidden" ! A.name fieldName ! A.value (cs $ inputValue False)
                                    Blaze.text fieldLabel
                                    validationResult
                                    helpText

                    renderTextField :: Blaze.AttributeValue -> FormField -> Blaze.Html
                    renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, labelClass, placeholder, required }) =
                        formGroup do
                            unless (disableLabel || null fieldLabel) [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                            let theInput = (fieldInput formField)
                                    ! A.type_ inputType
                                    ! A.name fieldName
                                    ! A.placeholder (cs placeholder)
                                    ! A.id (cs fieldInputId)
                                    ! A.class_ (cs $ classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))])
                                    !? (required, A.required "required")
                            if fieldValue == "" then theInput else theInput ! A.value (cs fieldValue)
                            validationResult
                            helpText

                    renderSelectField :: FormField -> Blaze.Html
                    renderSelectField formField@(FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disableLabel, disableValidationResult, fieldInput, labelClass, required }) =
                        formGroup do
                            unless disableLabel [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                            H.select ! A.name fieldName ! A.id (cs fieldInputId) ! A.class_ (cs $ classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]) ! A.value (cs fieldValue) !? (required, A.required "required") $ do
                                --Html5.option ! A.disabled "disabled" ! A.selected "selected" $ Html5.text ("Bitte auswählen" :: Text)
                                let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                                (if isValueSelected then Blaze.option else Blaze.option ! A.selected "selected")  ! A.disabled "disabled" $ Blaze.text (if null placeholder then "Please select" else placeholder)
                                forEach (options fieldType) $ \(optionLabel, optionValue) -> (let option = Blaze.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                            validationResult
                            helpText



            styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
            styledValidationResult cssFramework formField@(FormField { validatorResult = Just message }) =
                let className :: Text = get #styledValidationResultClass cssFramework
                in [hsx|<div class={className}>{message}</div>|]
            styledValidationResult _ _ = mempty

            styledValidationResultClass = ""

            styledSubmitButton cssFramework SubmitButton { label, buttonClass } =
                let className :: Text = get #styledSubmitButtonClass cssFramework
                in [hsx|<button class={classes [(className, True), (buttonClass, not (null buttonClass))]}>{label}</button>|]

            styledInputClass _ = ""
            styledInputInvalidClass _ = "invalid"

            styledFormGroupClass = ""

            styledFormFieldHelp _ FormField { helpText = "" } = mempty
            styledFormFieldHelp _ FormField { helpText } = [hsx|<p>{helpText}</p>|]

            styledSubmitButtonClass = ""

bootstrap :: CSSFramework
bootstrap = def { styledFlashMessage, styledSubmitButtonClass, styledFormGroupClass, styledFormFieldHelp, styledInputClass, styledInputInvalidClass, styledValidationResultClass }
    where
        styledFlashMessage _ (Session.SuccessFlashMessage message) = [hsx|<div class="alert alert-success">{message}</div>|]
        styledFlashMessage _ (Session.ErrorFlashMessage message) = [hsx|<div class="alert alert-danger">{message}</div>|]

        styledInputClass FormField {} = "form-control"
        styledInputInvalidClass _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text text-muted">{helpText}</small>|]

        styledFormGroupClass = "form-group"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"

tailwind :: CSSFramework
tailwind = def { styledFlashMessage, styledSubmitButtonClass, styledFormGroupClass, styledFormFieldHelp, styledInputClass, styledInputInvalidClass, styledValidationResultClass }
    where
        styledFlashMessage _ (Session.SuccessFlashMessage message) = [hsx|<div class="bg-teal-100 border-t-4 border-teal-500 rounded-b text-teal-900 px-4 py-3 shadow-md">{message}</div>|]
        styledFlashMessage _ (Session.ErrorFlashMessage message) = [hsx|<div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">{message}</div>|]

        styledInputClass FormField {} = "form-control"
        styledInputInvalidClass _ = "is-invalid"

        styledSubmitButtonClass = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<p class="text-gray-600 text-xs italic">{helpText}</p>|]

        styledFormGroupClass = "flex flex-wrap -mx-3 mb-6"

        styledValidationResultClass = "text-red-500 text-xs italic"