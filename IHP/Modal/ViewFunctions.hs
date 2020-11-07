{-|
Module: IHP.Modal.ViewFunctions
Description: View Helper Functions to use modals
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Modal.ViewFunctions (modal, renderModal) where

import IHP.Prelude
import IHP.Controller.Context
import IHP.HtmlSupport.QQ (hsx)
import IHP.Modal.Types
import Text.Blaze.Html5 (Html, preEscapedText)

renderModal modal = renderModal' modal True
renderModal' Modal { .. } show = [hsx|
        <div class={modalClassName} id="modal" tabindex="-1" role="dialog" aria-labelledby="exampleModalLabel" aria-hidden="true" style={displayStyle} onclick="if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()">
            {modalInner}
        </div>
        <a id="modal-backdrop" href={modalCloseUrl} class={backdropClassName} style={displayStyle}/>
    |]
        where
            modalClassName :: Text
            modalClassName = "modal fade overflow-auto " <> if show then "show" else ""
            backdropClassName :: Text
            backdropClassName = "modal-backdrop fade " <> if show then "show" else ""
            displayStyle :: Text
            displayStyle = if show then "display: block" else "display: none"

            modalInner = [hsx|
            <div class="modal-dialog" role="document" id="modal-inner">
                <div class="modal-content">
                    {renderModalHeader modalTitle modalCloseUrl}
                    <div class="modal-body">{modalContent}</div>
                    {renderModalFooter}
                </div>
            </div>
            |]

            renderModalFooter =
                case modalFooter of
                    Just modalFooter -> [hsx|<div class="modal-footer">{modalFooter}</div>|]
                    Nothing -> mempty

renderModalHeader :: Text -> Text -> Html
renderModalHeader title closeUrl = [hsx|
    <div class="modal-header">
      <h5 class="modal-title" id="exampleModalLabel">{title}</h5>
      <a href={closeUrl} class="btn-link close" data-dismiss="modal" aria-label="Close">
        <span aria-hidden="true">{preEscapedText "&times;"}</span>
      </a>
    </div>
|]

modal :: (?context :: ControllerContext) => Html
modal = 
    case maybeFromFrozenContext of
        Just (ModalContainer html) -> html
        Nothing -> mempty