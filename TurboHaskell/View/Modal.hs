module TurboHaskell.View.Modal (Modal (..), withModal, renderModalHeader) where

import ClassyPrelude
import TurboHaskell.HtmlSupport.ToHtml
import TurboHaskell.HtmlSupport.QQ
import Text.Blaze.Html5 (Html, preEscapedText)
import Control.Lens hiding ((|>))
import Data.Generics.Product

data Modal = Modal { modalContent :: Html, modalFooter :: Html, modalCloseUrl :: Text, modalTitle :: Text }

renderModal Modal { modalContent, modalFooter, modalCloseUrl, modalTitle } show = [hsx|
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
                {modalContent}
              </div>
            </div>
            |]

renderModalHeader :: Text -> Text -> Html
renderModalHeader title closeUrl = [hsx|
    <div class="modal-header">
      <h5 class="modal-title" id="exampleModalLabel">{title}</h5>
      <a href={closeUrl} class="btn-link close" data-dismiss="modal" aria-label="Close">
        <span aria-hidden="true">{preEscapedText "&times;"}</span>
      </a>
    </div>
|]

emptyModal = Modal { modalContent = [hsx|<div></div>|], modalCloseUrl = mempty }

instance ToHtml (Maybe Modal) where
    toHtml (Just modal) = renderModal modal True
    toHtml Nothing = renderModal emptyModal False

onClick :: Text
onClick = "if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()"

withModal :: (?viewContext :: viewContext, HasField' "modal" viewContext (Maybe Modal)) => Modal -> ((?viewContext :: viewContext) => Html) -> Html
withModal modal expr = let viewContext' = ?viewContext in let ?viewContext = setField @"modal" (Just modal) viewContext' in expr
