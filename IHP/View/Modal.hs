module IHP.View.Modal (Modal (..), setModal, renderModalHeader, initModal, renderCurrentModal, getCurrentModal) where

import IHP.Prelude
import IHP.HtmlSupport.ToHtml
import IHP.HtmlSupport.QQ
import Text.Blaze.Html5 (Html, preEscapedText)
import IHP.ControllerSupport
import IHP.Controller.Context

import qualified Data.TMap as TypeMap

data Modal = Modal { modalContent :: Html, modalFooter :: Html, modalCloseUrl :: Text, modalTitle :: Text }

renderModal Modal { modalContent, modalFooter, modalCloseUrl, modalTitle } show = [hsx|
        <div class={modalClassName} id="modal" tabindex="-1" role="dialog" aria-labelledby="exampleModalLabel" aria-hidden="true" style={displayStyle} onclick={onClick}>
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
                    <div class="modal-footer">{modalFooter}</div>
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

emptyModal = Modal { modalContent = [hsx|<div></div>|], modalCloseUrl = mempty, modalFooter = mempty, modalTitle = mempty }

instance ToHtml (Maybe Modal) where
    toHtml (Just modal) = renderModal modal True
    toHtml Nothing = renderModal emptyModal False

onClick :: Text
onClick = "if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()"

setModal :: (?context :: ControllerContext) => Html -> IO ()
setModal modal = do
    putContext (ModalContainer (Just modal))

getCurrentModal :: (?context :: ControllerContext) => IO (Maybe Html)
getCurrentModal = do
    (ModalContainer maybeHtml) <- fromContext @ModalContainer
    pure maybeHtml

newtype ModalContainer = ModalContainer (Maybe Html)

initModal :: (?context :: ControllerContext) => IO ()
initModal = do 
    putContext (ModalContainer Nothing)

renderCurrentModal :: (?context :: ControllerContext) => Html
renderCurrentModal = 
    let (ModalContainer maybeHtml) = fromFrozenContext
    in fromMaybe mempty maybeHtml
