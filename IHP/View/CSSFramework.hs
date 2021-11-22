{-|
Module: IHP.View.CSSFramework
Description: Adds support for bootstrap, tailwind, etc. to IHP
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework where

import IHP.Prelude
import IHP.FlashMessages.Types
import qualified Text.Blaze.Html5 as Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), (!?))
import qualified Text.Blaze.Html5.Attributes as A
import IHP.ModelSupport
import IHP.Breadcrumb.Types
import IHP.Pagination.Helpers
import IHP.Pagination.Types
import IHP.View.Types (PaginationView(linkPrevious, pagination))


-- | Provides an unstyled CSSFramework
--
-- This way we can later add more properties to the CSSFramework without having update all the CSS Frameworks manually
instance Default CSSFramework where
    def = CSSFramework
            {
                styledFlashMessage = \cssFramework -> \case
                        SuccessFlashMessage message -> [hsx|<div>{message}</div>|]
                        ErrorFlashMessage message -> [hsx|<div>{message}</div>|]
                , styledFlashMessages
                , styledFormField
                , styledTextFormField
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
            styledFlashMessages cssFramework flashMessages = forEach flashMessages (styledFlashMessage cssFramework cssFramework)

            styledFormField :: CSSFramework -> FormField -> Blaze.Html
            styledFormField cssFramework@CSSFramework {styledValidationResult, styledTextFormField, styledCheckboxFormField, styledSelectFormField} formField =
                formGroup renderInner
                where
                    renderInner = case get #fieldType formField of
                        TextInput -> styledTextFormField cssFramework "text" formField validationResult
                        NumberInput -> styledTextFormField cssFramework "number" formField validationResult
                        PasswordInput -> styledTextFormField cssFramework "password" formField validationResult
                        ColorInput -> styledTextFormField cssFramework "color" formField validationResult
                        EmailInput -> styledTextFormField cssFramework "email" formField validationResult
                        DateInput -> styledTextFormField cssFramework "date" formField validationResult
                        DateTimeInput -> styledTextFormField cssFramework "datetime-local" formField validationResult
                        CheckboxInput -> styledCheckboxFormField cssFramework formField validationResult
                        HiddenInput -> styledTextFormField cssFramework "hidden" formField { disableLabel = True, disableGroup = True, disableValidationResult = True } validationResult
                        TextareaInput -> styledTextFormField cssFramework "text" formField validationResult
                        SelectInput {} -> styledSelectFormField cssFramework formField validationResult
                        FileInput -> styledTextFormField cssFramework "file" formField validationResult

                    validationResult :: Blaze.Html
                    validationResult = unless (get #disableValidationResult formField) (styledValidationResult cssFramework formField)

                    -- | Wraps the input inside a @<div class="form-group">...</div>@ (unless @disableGroup = True@)
                    formGroup :: Blaze.Html -> Blaze.Html
                    formGroup renderInner = case formField of
                        FormField { disableGroup = True } -> renderInner
                        FormField { fieldInputId } -> styledFormGroup cssFramework fieldInputId renderInner


            styledFormGroup :: CSSFramework -> Text -> Blaze.Html -> Blaze.Html
            styledFormGroup cssFramework@CSSFramework {styledFormGroupClass} fieldInputId renderInner =
                [hsx|<div class={styledFormGroupClass} id={"form-group-" <> fieldInputId}>{renderInner}</div>|]

            styledCheckboxFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
            styledCheckboxFormField cssFramework@CSSFramework {styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus } validationResult = do
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
            styledTextFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} inputType formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, placeholder, required, autofocus } validationResult =
                [hsx|
                    {label}
                    <input
                        type={inputType}
                        name={fieldName}
                        placeholder={placeholder}
                        id={fieldInputId}
                        class={classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                        value={fieldValue}
                        required={required}
                        disabled={disabled}
                        autofocus={autofocus}
                    />

                    {validationResult}
                    {helpText}
              |]
                where
                    label = unless (disableLabel || null fieldLabel) [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                    inputClass = (styledInputClass cssFramework formField, True)
                    inputInvalidClass = styledInputInvalidClass cssFramework formField
                    helpText = styledFormFieldHelp cssFramework formField

            styledSelectFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
            styledSelectFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus } validationResult =
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

            styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
            styledValidationResult cssFramework formField@FormField { validatorResult = Just violation } =
                let
                    className :: Text = get #styledValidationResultClass cssFramework
                    message = case violation of
                        TextViolation text -> [hsx|{text}|]
                        HtmlViolation html -> H.preEscapedToHtml html
                in
                    [hsx|<div class={className}>{message}</div>|]
            styledValidationResult _ _ = mempty

            styledValidationResultClass = ""

            styledSubmitButton cssFramework SubmitButton { label, buttonClass } =
                let className :: Text = get #styledSubmitButtonClass cssFramework
                in [hsx|<button class={classes [(className, True), (buttonClass, not (null buttonClass))]}>{label}</button>|]

            styledInputClass _ _ = ""
            styledInputInvalidClass _ _ = "invalid"

            styledFormGroupClass = ""

            styledFormFieldHelp _ FormField { helpText = "" } = mempty
            styledFormFieldHelp _ FormField { helpText } = [hsx|<p>{helpText}</p>|]

            styledSubmitButtonClass = ""

            styledPagination :: CSSFramework -> PaginationView -> Blaze.Html
            styledPagination _ paginationView =
                [hsx|

                <div class="d-flex justify-content-md-center">
                    <nav aria-label="Page Navigator" class="mr-2">
                        <ul class="pagination">
                            {get #linkPrevious paginationView}
                            {get #pageDotDotItems paginationView}
                            {get #linkNext paginationView}
                        </ul>
                    </nav>

                    <div class="form-row">
                        <div class="col-auto mr-2">
                            <select class="custom-select" id="maxItemsSelect" onchange="window.location.href = this.options[this.selectedIndex].dataset.url">
                                {get #itemsPerPageSelector paginationView}
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
                        {get #breadcrumbItems breadcrumbsView}

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
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text text-muted">{helpText}</small>|]

        styledFormGroupClass = "form-group"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"

tailwind :: CSSFramework
tailwind = def
    { styledFlashMessage
    , styledTextFormField
    , styledCheckboxFormField
    , styledSelectFormField
    , styledSubmitButtonClass
    , styledFormGroupClass
    , styledFormFieldHelp
    , styledInputClass
    , styledInputInvalidClass
    , styledValidationResultClass
    , styledPagination
    , styledPaginationLinkPrevious
    , styledPaginationLinkNext
    , styledPaginationPageLink
    , styledPaginationDotDot
    , styledPaginationItemsPerPageSelector
    , styledBreadcrumb
    , styledBreadcrumbItem
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="bg-green-100 border border-green-500 text-green-900 px-4 py-3 rounded relative">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">{message}</div>|]

        styledCheckboxFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
        styledCheckboxFormField cssFramework@CSSFramework {styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus } validationResult = do
            [hsx|<div class="form-check">{element}</div>|]
            where
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField

                theInput = [hsx|
                            <div>
                                <input
                                    type="checkbox"
                                    name={fieldName}
                                    class={classes ["form-check-input", (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                                    id={fieldInputId}
                                    checked={fieldValue == "yes"}
                                    required={required}
                                    disabled={disabled}
                                    autofocus={autofocus}
                                />

                                <input type="hidden" name={fieldName} value={inputValue False} />
                            </div>
                        |]


                element = if disableLabel
                    then [hsx|<div class="flex flex-row space-x-2">
                                {theInput}
                                <div class="flex flex-col space-y-2">
                                    {validationResult}
                                    {helpText}
                                </div>
                            </div>
                        |]
                    else [hsx|
                            <div class="flex flex-row space-x-2">
                                {theInput}
                                <div class="flex flex-col">
                                    <label
                                        class={classes ["font-medium text-gray-700", ("form-check-label", labelClass == ""), (labelClass, labelClass /= "")]}
                                        for={fieldInputId}
                                    >
                                        {fieldLabel}
                                    </label>

                                    {validationResult}
                                    {helpText}
                                </div>
                            </div>
                        |]

        styledTextFormField :: CSSFramework -> Text -> FormField -> Blaze.Html -> Blaze.Html
        styledTextFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} inputType formField@FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, placeholder, required, autofocus } validationResult =
            [hsx|
                {label}
                <input
                    type={inputType}
                    name={fieldName}
                    placeholder={placeholder}
                    id={fieldInputId}
                    class={classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]}
                    value={fieldValue}
                    required={required}
                    disabled={disabled}
                    autofocus={autofocus}
                />

                {validationResult}
                {helpText}
            |]
            where
                twLabelClass = "font-medium text-gray-700" <> " " <> labelClass
                label = unless (disableLabel || null fieldLabel) [hsx|<label class={twLabelClass} for={fieldInputId}>{fieldLabel}</label>|]
                inputClass = (styledInputClass cssFramework formField, True)
                inputInvalidClass = styledInputInvalidClass cssFramework formField
                helpText = styledFormFieldHelp cssFramework formField

        styledSelectFormField :: CSSFramework -> FormField -> Blaze.Html -> Blaze.Html
        styledSelectFormField cssFramework@CSSFramework {styledInputClass, styledInputInvalidClass, styledFormFieldHelp} formField@FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus } validationResult =
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
                >
                    <option selected={not isValueSelected} disabled={True}>{placeholder}</option>
                    {forEach (options fieldType) (getOption)}
                </select>

                {validationResult}
                {helpText}
            |]
            where
                twLabelClass = "font-medium text-gray-700" <> " " <> labelClass
                label = unless disableLabel [hsx|<label class={twLabelClass} for={fieldInputId}>{fieldLabel}</label>|]
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


        styledInputClass _ FormField {} = "focus:ring-blue-500 focus:border-blue-500 block w-full border-gray-300 rounded-md"
        styledInputInvalidClass _ _ = "is-invalid"

        styledSubmitButtonClass = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<p class="text-gray-600 text-xs italic">{helpText}</p>|]

        styledFormGroupClass = "flex flex-col my-6 space-y-2"

        styledValidationResultClass = "text-red-500 text-xs italic"

        styledPagination :: CSSFramework -> PaginationView -> Blaze.Html
        styledPagination _ paginationView@PaginationView {pageUrl, pagination} =
            let
                currentPage = get #currentPage pagination

                previousPageUrl = if hasPreviousPage pagination then pageUrl $ currentPage - 1 else "#"
                nextPageUrl = if hasNextPage pagination then pageUrl $ currentPage + 1 else "#"

                defaultClass = "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                previousClass = classes
                    [ defaultClass
                    , ("disabled", not $ hasPreviousPage pagination)
                    ]
                nextClass = classes
                    [ defaultClass
                    , ("disabled", not $ hasNextPage pagination)
                    ]

                previousMobileOnly =
                    [hsx|
                        <a href={previousPageUrl} class={previousClass}>
                            Previous
                        </a>
                    |]

                nextMobileOnly =
                    [hsx|
                        <a href={nextPageUrl} class={nextClass}>
                            Next
                        </a>
                    |]

            in
            [hsx|
                <div class="bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6">
                    <div class="flex-1 flex justify-between sm:hidden">
                        {previousMobileOnly}
                        {nextMobileOnly}
                    </div>
                    <div class="hidden sm:flex-1 sm:flex sm:items-center sm:justify-between">
                        <div class="text-sm text-gray-700">
                            <select class="px-4 py-3" id="maxItemsSelect" onchange="window.location.href = this.options[this.selectedIndex].dataset.url">
                                {get #itemsPerPageSelector paginationView}
                            </select>
                        </div>
                        <div>
                        <nav class="relative z-0 inline-flex rounded-md shadow-sm -space-x-px" aria-label="Pagination">
                            {get #linkPrevious paginationView}

                            {get #pageDotDotItems paginationView}

                            {get #linkNext paginationView}
                        </nav>
                        </div>
                    </div>
                </div>
            |]

        styledPaginationLinkPrevious :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkPrevious _ pagination@Pagination {currentPage} pageUrl =
            let
                prevClass = classes
                    [ "relative inline-flex items-center px-2 py-2 rounded-l-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    , ("disabled", not $ hasPreviousPage pagination)
                    ]

                url = if hasPreviousPage pagination then pageUrl else "#"

            in
                [hsx|
                    <a href={url} class={prevClass}>
                        <span class="sr-only">Previous</span>
                        <!-- Heroicon name: solid/chevron-left -->
                        <svg class="h-5 w-5" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
                            <path fill-rule="evenodd" d="M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z" clip-rule="evenodd" />
                        </svg>
                    </a>
                |]

        styledPaginationLinkNext :: CSSFramework -> Pagination -> ByteString -> Blaze.Html
        styledPaginationLinkNext _ pagination@Pagination {currentPage} pageUrl =
            let
                nextClass = classes
                    [ "relative inline-flex items-center px-2 py-2 rounded-r-md border border-gray-300 bg-white text-sm font-medium text-gray-500 hover:bg-gray-50"
                    , ("disabled", not $ hasNextPage pagination)
                    ]

                url = if hasNextPage pagination then pageUrl else "#"
            in
                [hsx|
                    <a href={url} class={nextClass}>
                        <span class="sr-only">Next</span>
                        <!-- Heroicon name: solid/chevron-right -->
                        <svg class="h-5 w-5" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
                            <path fill-rule="evenodd" d="M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z" clip-rule="evenodd" />
                        </svg>
                    </a>

                |]

        styledPaginationPageLink :: CSSFramework -> Pagination -> ByteString -> Int -> Blaze.Html
        styledPaginationPageLink _ pagination@Pagination {currentPage} pageUrl pageNumber =
            let
                linkClass = classes
                    [ "relative inline-flex items-center px-4 py-2 border text-sm font-medium"
                    -- Current page
                    , ("z-10 bg-blue-50 border-blue-500 text-blue-600", pageNumber == currentPage)
                    -- Not current page
                    , ("bg-white border-gray-300 text-gray-500 hover:bg-gray-50", pageNumber /= currentPage)
                    ]
            in
                [hsx|
                    <a href={pageUrl} aria-current={pageNumber == currentPage} class={linkClass}>
                        {show pageNumber}
                    </a>
                |]


        styledPaginationDotDot :: CSSFramework -> Pagination -> Blaze.Html
        styledPaginationDotDot _ _ =
            [hsx|
                <span class="relative inline-flex items-center px-4 py-2 border border-gray-300 bg-white text-sm font-medium text-gray-700">
                    ...
                </span>
        |]


        styledPaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
        styledPaginationItemsPerPageSelector _ pagination@Pagination {pageSize} itemsPerPageUrl =
            let
                oneOption :: Int -> Blaze.Html
                oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
            in
                [hsx|{forEach [10,20,50,100,200] oneOption}|]


        styledBreadcrumb :: CSSFramework -> [BreadcrumbItem]-> BreadcrumbsView -> Blaze.Html
        styledBreadcrumb _ _ breadcrumbsView = [hsx|
            <nav class="breadcrumbs bg-white my-4" aria-label="Breadcrumb">
                <ol class="flex items-center space-x-2" role="list">
                    {get #breadcrumbItems breadcrumbsView}
                </ol>
            </nav>
        |]


        styledBreadcrumbItem :: CSSFramework -> [ BreadcrumbItem ]-> BreadcrumbItem -> Bool -> Blaze.Html
        styledBreadcrumbItem _ breadcrumbItems breadcrumbItem@BreadcrumbItem {breadcrumbLabel, url} isLast =
            let
                breadcrumbsClasses = classes ["flex flex-row space-x-2 text-gray-600 items-center", ("active", isLast)]

                -- Show chevron if item isn't the active one (i.e. the last one).
                chevronRight = unless isLast [hsx|
                <!-- heroicons.com chevron-right -->
                <svg xmlns="http://www.w3.org/2000/svg" class="flex-shrink-0 h-4 w-4 text-gray-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M9 5l7 7-7 7" />
                </svg>
                |]
            in
            case url of
                Nothing ->  [hsx|
                    <li class={breadcrumbsClasses}>
                        {breadcrumbLabel}
                        {chevronRight}
                    </li>
                |]
                Just url -> [hsx|
                    <li class={breadcrumbsClasses}>
                        <a class="hover:text-gray-700" href={url}>{breadcrumbLabel}</a>
                        {chevronRight}
                    </li>
                    |]