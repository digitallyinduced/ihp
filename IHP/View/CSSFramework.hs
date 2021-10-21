{-|
Module: IHP.View.CSSFramework
Description: Adds support for bootstrap, tailwind, etc. to IHP
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.View.CSSFramework where

import IHP.Prelude
import IHP.FlashMessages.Types
import qualified Text.Blaze.Html5 as Blaze
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml ()
import IHP.View.Types
import IHP.View.Classes

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), (!?))
import qualified Text.Blaze.Html5.Attributes as A
import IHP.ModelSupport
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
                , stylePaginationItemsPerPageSelector
                , styledPaginationLinkPrevious
                , styledPaginationLinkNext
            }
        where
            styledFlashMessages cssFramework flashMessages = forEach flashMessages (styledFlashMessage cssFramework cssFramework)

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
                        FileInput -> renderTextField "file" formField
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
                    renderCheckboxFormField formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus }) = do
                        formGroup do
                            (H.div ! A.class_ "form-check") do
                                let element = if disableLabel then H.div else H.label ! A.class_ (if labelClass == "" then "form-check-label" else H.textValue labelClass)
                                element do
                                    let theInput = H.input
                                            ! A.type_ "checkbox"
                                            ! A.name fieldName
                                            ! A.class_ (cs $ classes ["form-check-input", (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))])
                                            !? (required, A.required "required")
                                            !? (autofocus, A.autofocus "autofocus")
                                            !? (fieldValue == "yes", A.checked "checked")
                                            !? (disabled, A.disabled "disabled")
                                    theInput
                                    H.input ! A.type_ "hidden" ! A.name fieldName ! A.value (cs $ inputValue False)
                                    Blaze.text fieldLabel
                                    validationResult
                                    helpText

                    renderTextField :: Blaze.AttributeValue -> FormField -> Blaze.Html
                    renderTextField inputType formField@(FormField {fieldType, fieldName, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, placeholder, required, autofocus }) =
                        formGroup do
                            unless (disableLabel || null fieldLabel) [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                            let theInput = (fieldInput formField)
                                    ! A.type_ inputType
                                    ! A.name fieldName
                                    ! A.placeholder (cs placeholder)
                                    ! A.id (cs fieldInputId)
                                    ! A.class_ (cs $ classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))])
                                    !? (required, A.required "required")
                                    !? (autofocus, A.autofocus "autofocus")
                                    !? (disabled, A.disabled "disabled")
                            if fieldValue == "" then theInput else theInput ! A.value (cs fieldValue)
                            validationResult
                            helpText

                    renderSelectField :: FormField -> Blaze.Html
                    renderSelectField formField@(FormField {fieldType, fieldName, placeholder, fieldLabel, fieldValue, fieldInputId, validatorResult, fieldClass, disabled, disableLabel, disableValidationResult, fieldInput, labelClass, required, autofocus }) =
                        formGroup do
                            unless disableLabel [hsx|<label class={labelClass} for={fieldInputId}>{fieldLabel}</label>|]
                            H.select ! A.name fieldName ! A.id (cs fieldInputId) ! A.class_ (cs $ classes [inputClass, (inputInvalidClass, isJust validatorResult), (fieldClass, not (null fieldClass))]) ! A.value (cs fieldValue) !? (disabled, A.disabled "disabled") !? (required, A.required "required") !? (autofocus, A.autofocus "autofocus") $ do
                                let isValueSelected = isJust $ find (\(optionLabel, optionValue) -> optionValue == fieldValue) (options fieldType)
                                (if isValueSelected then Blaze.option else Blaze.option ! A.selected "selected")  ! A.disabled "disabled" $ Blaze.text placeholder
                                forEach (options fieldType) $ \(optionLabel, optionValue) -> (let option = Blaze.option ! A.value (cs optionValue) in (if optionValue == fieldValue then option ! A.selected "selected" else option) $ cs optionLabel)
                            validationResult
                            helpText



            styledValidationResult :: CSSFramework -> FormField -> Blaze.Html
            styledValidationResult cssFramework formField@(FormField { validatorResult = Just violation }) =
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

            styledInputClass _ = ""
            styledInputInvalidClass _ = "invalid"

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

            stylePaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
            stylePaginationItemsPerPageSelector _ pagination@Pagination {pageSize} itemsPerPageUrl =
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

        styledInputClass FormField { fieldType = FileInput } = "form-control-file"
        styledInputClass FormField {} = "form-control"
        styledInputInvalidClass _ = "is-invalid"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<small class="form-text text-muted">{helpText}</small>|]

        styledFormGroupClass = "form-group"

        styledValidationResultClass = "invalid-feedback"

        styledSubmitButtonClass = "btn btn-primary"

tailwind :: CSSFramework
tailwind = def
    { styledFlashMessage
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
    , stylePaginationItemsPerPageSelector
    }
    where
        styledFlashMessage _ (SuccessFlashMessage message) = [hsx|<div class="bg-green-100 border border-green-500 text-green-900 px-4 py-3 rounded relative">{message}</div>|]
        styledFlashMessage _ (ErrorFlashMessage message) = [hsx|<div class="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative">{message}</div>|]

        styledInputClass FormField {} = "form-control"
        styledInputInvalidClass _ = "is-invalid"

        styledSubmitButtonClass = "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"

        styledFormFieldHelp _ FormField { helpText = "" } = mempty
        styledFormFieldHelp _ FormField { helpText } = [hsx|<p class="text-gray-600 text-xs italic">{helpText}</p>|]

        styledFormGroupClass = "flex flex-wrap -mx-3 mb-6"

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
                            <!-- @todo: Would be nice to keep to not have to duplicate onchange  -->
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
                    , ("z-10 bg-indigo-50 border-indigo-500 text-indigo-600", pageNumber == currentPage)
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


        stylePaginationItemsPerPageSelector :: CSSFramework -> Pagination -> (Int -> ByteString) -> Blaze.Html
        stylePaginationItemsPerPageSelector _ pagination@Pagination {pageSize} itemsPerPageUrl =
            let
                oneOption :: Int -> Blaze.Html
                oneOption n = [hsx|<option value={show n} selected={n == pageSize} data-url={itemsPerPageUrl n}>{n} items per page</option>|]
            in
                [hsx|{forEach [10,20,50,100,200] oneOption}|]
