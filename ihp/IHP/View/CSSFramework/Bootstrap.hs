{-|
Module: IHP.View.CSSFramework.Bootstrap
Description: Bootstrap CSS framework implementations
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework.Bootstrap (bootstrap, bootstrap4) where

import Prelude hiding (null)
import Data.Text (Text, null)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Default (def)
import Control.Monad (unless)
import IHP.HaskellSupport (forEach)
import IHP.ModelSupport.Types (Violation(..))
import IHP.InputValue (inputValue)
import qualified Text.Blaze.Html5 as Blaze
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes
import IHP.Breadcrumb.Types
import IHP.Pagination.Helpers
import IHP.Pagination.Types
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
import IHP.View.CSSFramework.Unstyled ()

bootstrap :: CSSFramework
bootstrap = def
    { styledFlashMessage
    , styledSubmitButtonClass
    , styledFormGroupClass
    , styledFormFieldHelp
    , styledInputClass
    , styledInputInvalidClass
    , styledValidationResultClass
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="alert alert-success">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="alert alert-danger">{message}</div>|]

        styledInputClass _ FormField { fieldType = FileInput } = "form-control-file"
        styledInputClass _ FormField {} = "form-control"
        styledInputInvalidClass _ _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text">{helpText}</small>|]

        styledFormGroupClass = "mb-3"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"

bootstrap4 :: CSSFramework
bootstrap4 = def
    { styledFlashMessage
    , styledFormField
    , styledTextFormField
    , styledTextareaFormField
    , styledCheckboxFormField
    , styledSelectFormField
    , styledFormGroup
    , styledSubmitButton
    , styledSubmitButtonClass
    , styledFormFieldHelp
    , styledInputClass
    , styledInputInvalidClass
    , styledFormGroupClass
    , styledValidationResult
    , styledValidationResultClass
    , styledPagination
    , styledPaginationPageLink
    , styledPaginationDotDot
    , styledPaginationItemsPerPageSelector
    , styledPaginationLinkPrevious
    , styledPaginationLinkNext
    , styledBreadcrumb
    , styledBreadcrumbItem
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="alert alert-success">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="alert alert-danger">{message}</div>|]

        styledInputClass _ FormField { fieldType = FileInput } = "form-control-file"
        styledInputClass _ FormField {} = "form-control"
        styledInputInvalidClass _ _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text text-muted">{helpText}</small>|]

        styledFormGroupClass = "form-group"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"


        styledFormField :: CSSFramework -> FormField -> Blaze.Html
        styledFormField cssFramework@CSSFramework {styledValidationResult, styledTextFormField, styledCheckboxFormField, styledSelectFormField, styledRadioFormField, styledTextareaFormField} formField =
            formGroup renderInner
            where
                renderInner = case formField.fieldType of
                    TextInput -> styledTextFormField cssFramework "text" formField validationResult
                    NumberInput -> styledTextFormField cssFramework "number" formField validationResult
                    PasswordInput -> styledTextFormField cssFramework "password" formField validationResult
                    ColorInput -> styledTextFormField cssFramework "color" formField validationResult
                    EmailInput -> styledTextFormField cssFramework "email" formField validationResult
                    DateInput -> styledTextFormField cssFramework "date" formField validationResult
                    DateTimeInput -> styledTextFormField cssFramework "datetime-local" formField validationResult
                    UrlInput -> styledTextFormField cssFramework "url" formField validationResult
                    CheckboxInput -> styledCheckboxFormField cssFramework formField validationResult
                    HiddenInput -> styledTextFormField cssFramework "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True } validationResult
                    TextareaInput -> styledTextareaFormField cssFramework formField validationResult
                    SelectInput {} -> styledSelectFormField cssFramework formField validationResult
                    RadioInput {} -> styledRadioFormField cssFramework formField validationResult
                    FileInput -> styledTextFormField cssFramework "file" formField validationResult

                validationResult :: Blaze.Html
                validationResult = unless formField.disableValidationResult (styledValidationResult cssFramework formField)

                -- | Wraps the input inside a @<div class="form-group">...</div>@ (unless @disableGroup = True@)
                formGroup :: Blaze.Html -> Blaze.Html
                formGroup renderInner = case formField of
                    FormField { disableGroup = True } -> renderInner
                    FormField { fieldInputId } -> styledFormGroup cssFramework fieldInputId renderInner


        styledFormGroup :: CSSFramework -> Text -> Blaze.Html -> Blaze.Html
        styledFormGroup cssFramework@CSSFramework {styledFormGroupClass} fieldInputId renderInner =
            [hsx|<div class={styledFormGroupClass} id={"form-group-" <> fieldInputId}>{renderInner}</div>|]

        styledCheckboxFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
        styledCheckboxFormField cssFramework@CSSFramework {styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, required, autofocus } validationResult = do
            [hsx|<div class="form-check">{element}</div>|]
            where
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField

                -- If the checkbox is checked off, the browser will not send the parameter as part of the form.
                -- This will then make it impossible to set a field to False using a checkbox.
                -- For that we add the "hidden" input type.
                theInput = [hsx|
                                <input
                                    type="checkbox"
                                    name={fieldName}
                                    class={classes ["form-check-input", (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                                    id={fieldInputId}
                                    checked={fieldValue == "yes"}
                                    required={required}
                                    disabled={disabled}
                                    autofocus={autofocus}
                                    {...additionalAttributes}
                                />

                                <input type="hidden" name={fieldName} value={inputValue False} />
                        |]


                element = if disableLabel
                    then [hsx|<div>
                                {theInput}
                                {validationResult}
                                {helpText}
                            </div>
                        |]
                    else [hsx|
                            {theInput}
                            <label
                                class={classes [("form-check-label", labelClass == ""), (labelClass, labelClass /= "")]}
                                for={fieldInputId}
                            >
                                {fieldLabel}
                            </label>

                            {validationResult}
                            {helpText}
                        |]

        styledTextFormField :: CSSFramework -> Text -> FormField -> Blaze.Html -> Blaze.Html
        styledTextFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} inputType formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, placeholder, required, autofocus } validationResult =
            [hsx|
                {label}
                <input
                    type={inputType}
                    name={fieldName}
                    placeholder={placeholder}
                    id={fieldInputId}
                    class={classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                    value={maybeValue}
                    required={required}
                    disabled={disabled}
                    autofocus={autofocus}
                    {...additionalAttributes}
                />

                {validationResult}
                {helpText}
          |]
            where
                label = unless (disableLabel || null fieldLabel) [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                inputClass = (styledInputClass cssFramework formField, True)
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField
                -- If there's no value, then we want to hide the "value" attribute.
                -- Exception: date and datetime inputs need the value attribute even when empty
                -- for HTML5 validation to work properly with the required attribute.
                maybeValue = if fieldValue == "" && inputType /= "date" && inputType /= "datetime-local"
                    then Nothing
                    else Just fieldValue

        styledSelectFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
        styledSelectFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, required, autofocus } validationResult =
            [hsx|
                {label}
                <select
                    name={fieldName}
                    id={fieldInputId}
                    class={classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                    value={fieldValue}
                    disabled={disabled}
                    required={required}
                    autofocus={autofocus}
                    {...additionalAttributes}
                >
                    <option selected={not isValueSelected} disabled={True}>{placeholder}</option>
                    {forEach (options fieldType) (getOption)}
                </select>

                {validationResult}
                {helpText}
            |]
            where
                label = unless disableLabel [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                inputClass = (styledInputClass cssFramework formField, True)
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField

                isValueSelected = any (\(_, optionValue) -> optionValue == fieldValue) (options fieldType)

                -- Get a single option.
                getOption (optionLabel, optionValue) = [hsx|
                    <option value={optionValue} selected={optionValue == fieldValue}>
                        {optionLabel}
                    </option>
                |]

        styledTextareaFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
        styledTextareaFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, placeholder, required, autofocus } validationResult =
            [hsx|
                {label}
                <textarea
                    name={fieldName}
                    placeholder={placeholder}
                    id={fieldInputId}
                    class={classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                    required={required}
                    disabled={disabled}
                    autofocus={autofocus}
                    {...additionalAttributes}
                >{fieldValue}</textarea>{validationResult}{helpText}|]
            where
                label = unless (disableLabel || null fieldLabel) [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                inputClass = (styledInputClass cssFramework formField, True)
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField

        styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
        styledValidationResult cssFramework formField@FormField { validatorResult = Just violation } =
            let
                className :: Text = cssFramework.styledValidationResultClass
                message = case violation of
                    TextViolation text -> [hsx|{text}|]
                    HtmlViolation html -> Blaze.preEscapedToHtml html
            in
                [hsx|<div class={className}>{message}</div>|]
        styledValidationResult _ _ = mempty

        styledSubmitButton cssFramework SubmitButton { label, buttonClass, buttonDisabled } =
            let className :: Text = cssFramework.styledSubmitButtonClass
            in [hsx|<button class={classes [(className, True), (buttonClass, not (null buttonClass))]} disabled={buttonDisabled} type="submit">{label}</button>|]

        styledPagination :: CSSFramework -> PaginationView -> Blaze.Html
        styledPagination _ paginationView =
            [hsx|

            <div class="d-flex justify-content-md-center">
                <nav aria-label="Page Navigator" class="mr-2">
                    <ul class="pagination">
                        {paginationView.linkPrevious}
                        {paginationView.pageDotDotItems}
                        {paginationView.linkNext}
                    </ul>
                </nav>

                <div class="form-row">
                    <div class="col-auto mr-2">
                        <select class="custom-select" id="maxItemsSelect" onchange="window.location.href = this.options[this.selectedIndex].dataset.url">
                            {paginationView.itemsPerPageSelector}
                        </select>
                    </div>
                </div>

            </div>
            |]

        styledPaginationPageLink :: CSSFramework -> Pagination -> ByteString -> Int -> Blaze.Html
        styledPaginationPageLink _ pagination@Pagination {currentPage} pageUrl pageNumber =
            let
                linkClass = classes ["page-item", ("active", pageNumber == currentPage)]
            in
                [hsx|<li class={linkClass}><a class="page-link" href={pageUrl}>{show pageNumber}</a></li>|]


        styledPaginationDotDot :: CSSFramework -> Pagination -> Blaze.Html
        styledPaginationDotDot _ _ =
            [hsx|<li class="page-item"><a class="page-link">…</a></li>|]

        styledPaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
        styledPaginationItemsPerPageSelector _ pagination@Pagination {pageSize} itemsPerPageUrl =
            let
                oneOption :: Int -> Blaze.Html
                oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
            in
                [hsx|{forEach [10,20,50,100,200] oneOption}|]

        styledPaginationLinkPrevious :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkPrevious _ pagination@Pagination {currentPage} pageUrl =
            let
                prevClass = classes ["page-item", ("disabled", not $ hasPreviousPage pagination)]
                url = if hasPreviousPage pagination then pageUrl else "#"
            in
                [hsx|
                    <li class={prevClass}>
                        <a class="page-link" href={url} aria-label="Previous">
                            <span aria-hidden="true">&laquo;</span>
                            <span class="sr-only">Previous</span>
                        </a>
                    </li>
                |]

        styledPaginationLinkNext :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkNext _ pagination@Pagination {currentPage} pageUrl =
            let
                nextClass = classes ["page-item", ("disabled", not $ hasNextPage pagination)]
                url = if hasNextPage pagination then pageUrl else "#"
            in
                [hsx|
                    <li class={nextClass}>
                        <a class="page-link" href={url} aria-label="Next">
                            <span aria-hidden="true">&raquo;</span>
                            <span class="sr-only">Next</span>
                        </a>
                    </li>
                |]

        styledBreadcrumb :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbsView -> Blaze.Html
        styledBreadcrumb _ _ breadcrumbsView = [hsx|
            <nav>
                <ol class="breadcrumb">
                    {breadcrumbsView.breadcrumbItems}

                </ol>
            </nav>
        |]


        styledBreadcrumbItem :: CSSFramework -> [ BreadcrumbItem ]-> BreadcrumbItem -> Bool -> Blaze.Html
        styledBreadcrumbItem _ breadcrumbItems breadcrumbItem@BreadcrumbItem {breadcrumbLabel, url} isLast =
            let
                breadcrumbsClasses = classes ["breadcrumb-item", ("active", isLast)]
            in
            case url of
                Nothing ->  [hsx|<li class={breadcrumbsClasses}>{breadcrumbLabel}</li>|]
                Just url -> [hsx|<li class={breadcrumbsClasses}><a href={url}>{breadcrumbLabel}</a></li>|]
