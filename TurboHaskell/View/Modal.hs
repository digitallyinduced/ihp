module TurboHaskell.View.Modal (Modal (..), setModal, renderModalHeader, initModal, renderCurrentModal, getCurrentModal) where

import TurboHaskell.Prelude
import TurboHaskell.HtmlSupport.ToHtml
import TurboHaskell.HtmlSupport.QQ
import Text.Blaze.Html5 (Html, preEscapedText)
import TurboHaskell.ControllerSupport

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

emptyModal = Modal { modalContent = [hsx|<div></div>|], modalCloseUrl = mempty, modalFooter = mempty, modalTitle = mempty }

instance ToHtml (Maybe Modal) where
    toHtml (Just modal) = renderModal modal True
    toHtml Nothing = renderModal emptyModal False

onClick :: Text
onClick = "if (event.target.id === 'modal') document.getElementById('modal-backdrop').click()"

setModal :: (?controllerContext :: ControllerContext) => Html -> IO ()
setModal modal = do
    let (ModalContainer ref) = fromControllerContext @ModalContainer
    writeIORef ref (Just modal)
    pure ()

getCurrentModal :: (?controllerContext :: ControllerContext) => IO (Maybe Html)
getCurrentModal = do
    let (ModalContainer ref) = fromControllerContext @ModalContainer
    readIORef ref

newtype ModalContainer = ModalContainer (IORef (Maybe Html))
initModal context = do 
    modalContainer <- ModalContainer <$> newIORef Nothing
    pure (TypeMap.insert @ModalContainer modalContainer context)

renderCurrentModal :: (?viewContext :: viewContext, HasField "modal" viewContext (Maybe Html)) => Html
renderCurrentModal = 
    let controllerContext :: (Maybe Html) = getField @"modal" ?viewContext
    in fromMaybe mempty controllerContext
