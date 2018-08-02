module Foundation.View.Modal (Modal (..)) where

import ClassyPrelude
import Foundation.HtmlSupport.ToHtml
import Foundation.HtmlSupport.QQ
import Text.Blaze.Html5 (Html, preEscapedText)

data Modal = Modal { modalBody :: Html, modalFooter :: Html, modalCloseUrl :: Text, modalTitle :: Text }

renderModal Modal { modalBody, modalFooter, modalCloseUrl, modalTitle } show = [hsx|
        <div class={modalClassName} id="modal" tabindex="-1" role="dialog" aria-labelledby="exampleModalLabel" aria-hidden="true" style={displayStyle} onclick={onClick}>
            {modalInner}
        </div>
        <a id="modal-backdrop" href={modalCloseUrl} class={backdropClassName} style={displayStyle}/>
    |]
        where
            modalClassName :: Text
            modalClassName = "modal fade " <> if show then "show" else ""
            backdropClassName :: Text
            backdropClassName = "modal-backdrop fade " <> if show then "show" else ""
            displayStyle :: Text
            displayStyle = if show then "display: block" else "display: none"

            modalInner = [hsx|
            <div class="modal-dialog" role="document" id="modal-inner">
              <div class="modal-content">
                <div class="modal-header">
                  <h5 class="modal-title" id="exampleModalLabel">{modalTitle}</h5>
                  <a href={modalCloseUrl} class="btn-link close" data-dismiss="modal" aria-label="Close">
                    <span aria-hidden="true">{preEscapedText "&times;"}</span>
                  </a>
                </div>
                <div class="modal-body">
                  {modalBody}
                </div>
                <div class="modal-footer">
                  {modalFooter}
                </div>
              </div>
            </div>
            |]

emptyModal = Modal { modalBody = mempty, modalFooter = mempty, modalCloseUrl = mempty, modalTitle = mempty }

instance ToHtml (Maybe Modal) where
    toHtml (Just modal) = renderModal modal True
    toHtml Nothing = renderModal emptyModal False

onClick :: Text
onClick = "if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()"
