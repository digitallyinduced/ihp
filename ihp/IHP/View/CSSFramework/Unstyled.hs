{-|
Module: IHP.View.CSSFramework.Unstyled
Description: Unstyled CSSFramework (base implementation)
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework.Unstyled
( styledFlashMessageDefault
, styledFlashMessagesDefault
, styledFormFieldDefault
, styledFormGroupDefault
, styledCheckboxFormFieldDefault
, styledTextFormFieldDefault
, styledSelectFormFieldDefault
, styledRadioFormFieldDefault
, styledTextareaFormFieldDefault
, styledValidationResultDefault
, styledSubmitButtonDefault
, styledFormFieldHelpDefault
, styledInputClassDefault
, styledInputInvalidClassDefault
, styledPaginationDefault
, styledPaginationPageLinkDefault
, styledPaginationDotDotDefault
, styledPaginationItemsPerPageSelectorDefault
, styledPaginationLinkPreviousDefault
, styledPaginationLinkNextDefault
, styledBreadcrumbDefault
, styledBreadcrumbItemDefault
) where

import IHP.Prelude
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
import qualified Text.Blaze.Html5 as Blaze
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes

import IHP.ModelSupport
import IHP.Breadcrumb.Types
import IHP.Pagination.Helpers
import IHP.Pagination.Types

-- | Provides an unstyled CSSFramework
--
-- This way we can later add more properties to the CSSFramework without having to update all the CSS Frameworks manually
instance Default CSSFramework where
    def = CSSFramework
            { styledFlashMessage = styledFlashMessageDefault
            , styledFlashMessages = styledFlashMessagesDefault
            , styledFormField = styledFormFieldDefault
            , styledTextFormField = styledTextFormFieldDefault
            , styledTextareaFormField = styledTextareaFormFieldDefault
            , styledCheckboxFormField = styledCheckboxFormFieldDefault
            , styledSelectFormField = styledSelectFormFieldDefault
            , styledRadioFormField = styledRadioFormFieldDefault
            , styledFormGroup = styledFormGroupDefault
            , styledSubmitButton = styledSubmitButtonDefault
            , styledSubmitButtonClass = ""
            , styledFormFieldHelp = styledFormFieldHelpDefault
            , styledInputClass = styledInputClassDefault
            , styledInputInvalidClass = styledInputInvalidClassDefault
            , styledFormGroupClass = ""
            , styledValidationResult = styledValidationResultDefault
            , styledValidationResultClass = ""
            , styledPagination = styledPaginationDefault
            , styledPaginationPageLink = styledPaginationPageLinkDefault
            , styledPaginationDotDot = styledPaginationDotDotDefault
            , styledPaginationItemsPerPageSelector = styledPaginationItemsPerPageSelectorDefault
            , styledPaginationLinkPrevious = styledPaginationLinkPreviousDefault
            , styledPaginationLinkNext = styledPaginationLinkNextDefault
            , styledBreadcrumb = styledBreadcrumbDefault
            , styledBreadcrumbItem = styledBreadcrumbItemDefault
            }

styledFlashMessageDefault :: CSSFramework -> FlashMessage -> Blaze.Html
styledFlashMessageDefault cssFramework = \case
    SuccessFlashMessage message -> [hsx|<div>{message}</div>|]
    ErrorFlashMessage message -> [hsx|<div>{message}</div>|]

styledFlashMessagesDefault :: CSSFramework -> [FlashMessage] -> Blaze.Html
styledFlashMessagesDefault cssFramework flashMessages = forEach flashMessages (cssFramework.styledFlashMessage cssFramework)

styledFormFieldDefault :: CSSFramework -> FormField -> Blaze.Html
styledFormFieldDefault cssFramework@CSSFramework {styledValidationResult, styledTextFormField, styledCheckboxFormField, styledSelectFormField, styledRadioFormField, styledTextareaFormField, styledFormGroup} formField =
    formGroup renderInner
    where
        renderInner = case formField.fieldType of
            TextInput -> styledTextFormField cssFramework "text" formField validationResult
            NumberInput -> styledTextFormField cssFramework "number" formField validationResult
            UrlInput -> styledTextFormField cssFramework "url" formField validationResult
            PasswordInput -> styledTextFormField cssFramework "password" formField validationResult
            ColorInput -> styledTextFormField cssFramework "color" formField validationResult
            EmailInput -> styledTextFormField cssFramework "email" formField validationResult
            DateInput -> styledTextFormField cssFramework "date" formField validationResult
            DateTimeInput -> styledTextFormField cssFramework "datetime-local" formField validationResult
            CheckboxInput -> styledCheckboxFormField cssFramework formField validationResult
            HiddenInput -> styledTextFormField cssFramework "hidden" formField validationResult
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

styledFormGroupDefault :: CSSFramework -> Text -> Blaze.Html -> Blaze.Html
styledFormGroupDefault cssFramework@CSSFramework {styledFormGroupClass} fieldInputId renderInner =
    [hsx|<div class={styledFormGroupClass} id={"form-group-" <> fieldInputId}>{renderInner}</div>|]

styledCheckboxFormFieldDefault :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
styledCheckboxFormFieldDefault cssFramework@CSSFramework {styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, required, autofocus } validationResult = do
    [hsx|<div class="form-check">{element}</div>|]
    where
        inputInvalidClass = styledInputInvalidClass cssFramework formField
        helpText = styledFormFieldHelp cssFramework formField

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

styledTextFormFieldDefault :: CSSFramework -> Text -> FormField -> Blaze.Html -> Blaze.Html
styledTextFormFieldDefault cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} inputType formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, placeholder, required, autofocus } validationResult =
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
        label = unless (disableLabel || null fieldLabel) [hsx|<label class={classes ["form-label", (labelClass, labelClass /= "")]} for={fieldInputId}>{fieldLabel}</label>|]
        inputClass = (styledInputClass cssFramework formField, True)
        inputInvalidClass = styledInputInvalidClass cssFramework formField
        helpText = styledFormFieldHelp cssFramework formField
        maybeValue = if fieldValue == "" && inputType /= "date" && inputType /= "datetime-local"
            then Nothing
            else Just fieldValue

styledSelectFormFieldDefault :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
styledSelectFormFieldDefault cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, required, autofocus } validationResult =
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
            <option value="" selected={not isValueSelected} disabled={True}>{placeholder}</option>
            {forEach (options fieldType) (getOption)}
        </select>

        {validationResult}
        {helpText}
    |]
    where
        label = unless disableLabel [hsx|<label class={classes ["form-label", (labelClass, labelClass /= "")]} for={fieldInputId}>{fieldLabel}</label>|]
        inputClass = (styledInputClass cssFramework formField, True)
        inputInvalidClass = styledInputInvalidClass cssFramework formField
        helpText = styledFormFieldHelp cssFramework formField

        isValueSelected = any (\(_, optionValue) -> optionValue == fieldValue) (options fieldType)

        getOption (optionLabel, optionValue) = [hsx|
            <option value={optionValue} selected={optionValue == fieldValue}>
                {optionLabel}
            </option>
        |]

styledRadioFormFieldDefault :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
styledRadioFormFieldDefault cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, required, autofocus } validationResult =
    [hsx|
        {label}
        <fieldset>
            {forEach (options fieldType) (getRadio)}
        </fieldset>

        {validationResult}
        {helpText}
    |]
    where
        label = unless disableLabel [hsx|<label class={classes ["form-label", (labelClass, labelClass /= "")]} for={fieldInputId}>{fieldLabel}</label>|]
        inputInvalidClass = styledInputInvalidClass cssFramework formField
        helpText = styledFormFieldHelp cssFramework formField

        getRadio (optionLabel, optionValue) = [hsx|
            <div class="form-check">
                <input
                    class={classes ["form-check-input", (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                    type="radio"
                    id={optionId}
                    name={fieldName}
                    value={optionValue}
                    checked={optionValue == fieldValue}
                    disabled={disabled}
                    required={required}
                    autofocus={autofocus}
                    {...additionalAttributes}
                />
                {radioLabel}
            </div>
        |]
            where
                optionId = fieldInputId <> "_" <> optionValue
                radioLabel = unless disableLabel [hsx|<label class={classes ["form-check-label", (labelClass, labelClass /= "")]} for={optionId}>{optionLabel}</label>|]

styledTextareaFormFieldDefault :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
styledTextareaFormFieldDefault cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, additionalAttributes, labelClass, placeholder, required, autofocus } validationResult =
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
        label = unless (disableLabel || null fieldLabel) [hsx|<label class={classes ["form-label", (labelClass, labelClass /= "")]} for={fieldInputId}>{fieldLabel}</label>|]
        inputClass = (styledInputClass cssFramework formField, True)
        inputInvalidClass = styledInputInvalidClass cssFramework formField
        helpText = styledFormFieldHelp cssFramework formField

styledValidationResultDefault :: CSSFramework -> FormField -> Blaze.Html
styledValidationResultDefault cssFramework formField@FormField { validatorResult = Just violation } =
    let
        className :: Text = cssFramework.styledValidationResultClass
        message = case violation of
            TextViolation text -> [hsx|{text}|]
            HtmlViolation html -> Blaze.preEscapedToHtml html
    in
        [hsx|<div class={className}>{message}</div>|]
styledValidationResultDefault _ _ = mempty

styledSubmitButtonDefault :: CSSFramework -> SubmitButton -> Blaze.Html
styledSubmitButtonDefault cssFramework SubmitButton { label, buttonClass, buttonDisabled } =
    let className :: Text = cssFramework.styledSubmitButtonClass
    in [hsx|<button class={classes [(className, True), (buttonClass, not (null buttonClass))]} disabled={buttonDisabled} type="submit">{label}</button>|]

styledFormFieldHelpDefault :: CSSFramework -> FormField -> Blaze.Html
styledFormFieldHelpDefault _ FormField { helpText = "" } = mempty
styledFormFieldHelpDefault _ FormField { helpText } = [hsx|<p>{helpText}</p>|]

styledInputClassDefault :: CSSFramework -> FormField -> Text
styledInputClassDefault _ _ = ""

styledInputInvalidClassDefault :: CSSFramework -> FormField -> Text
styledInputInvalidClassDefault _ _ = "invalid"

styledPaginationDefault :: CSSFramework -> PaginationView -> Blaze.Html
styledPaginationDefault _ paginationView =
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

styledPaginationPageLinkDefault :: CSSFramework -> Pagination -> ByteString -> Int -> Blaze.Html
styledPaginationPageLinkDefault _ pagination@Pagination {currentPage} pageUrl pageNumber =
    let
        linkClass = classes ["page-item", ("active", pageNumber == currentPage)]
    in
        [hsx|<li class={linkClass}><a class="page-link" href={pageUrl}>{show pageNumber}</a></li>|]

styledPaginationDotDotDefault :: CSSFramework -> Pagination -> Blaze.Html
styledPaginationDotDotDefault _ _ =
    [hsx|<li class="page-item"><a class="page-link">…</a></li>|]

styledPaginationItemsPerPageSelectorDefault :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
styledPaginationItemsPerPageSelectorDefault _ pagination@Pagination {pageSize} itemsPerPageUrl =
    let
        oneOption :: Int -> Blaze.Html
        oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
    in
        [hsx|{forEach [10,20,50,100,200] oneOption}|]

styledPaginationLinkPreviousDefault :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
styledPaginationLinkPreviousDefault _ pagination@Pagination {currentPage} pageUrl =
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

styledPaginationLinkNextDefault :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
styledPaginationLinkNextDefault _ pagination@Pagination {currentPage} pageUrl =
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

styledBreadcrumbDefault :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbsView -> Blaze.Html
styledBreadcrumbDefault _ _ breadcrumbsView = [hsx|
    <nav>
        <ol class="breadcrumb">
            {breadcrumbsView.breadcrumbItems}

        </ol>
    </nav>
|]

styledBreadcrumbItemDefault :: CSSFramework -> [ BreadcrumbItem ]-> BreadcrumbItem -> Bool -> Blaze.Html
styledBreadcrumbItemDefault _ breadcrumbItems breadcrumbItem@BreadcrumbItem {breadcrumbLabel, url} isLast =
    let
        breadcrumbsClasses = classes ["breadcrumb-item", ("active", isLast)]
    in
    case url of
        Nothing ->  [hsx|<li class={breadcrumbsClasses}>{breadcrumbLabel}</li>|]
        Just url -> [hsx|<li class={breadcrumbsClasses}><a href={url}>{breadcrumbLabel}</a></li>|]
