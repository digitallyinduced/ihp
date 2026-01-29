module Network.Wai.Middleware.FlashMessages
( setSuccessMessage
, setErrorMessage
, getSuccessMessage
, clearSuccessMessage
, consumeFlashMessagesMiddleware
, FlashMessage (..)
, requestFlashMessages
)
where

import Prelude
import Network.Wai
import Data.ByteString
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Session as Session
import qualified Data.Maybe as Maybe
import Data.Text (Text)

data FlashMessage
    = SuccessFlashMessage !Text
    | ErrorFlashMessage !Text

type SessionVaultKey = Vault.Key (Session.Session IO ByteString ByteString)
type FlashVaultKey = Vault.Key [FlashMessage]

-- | Sets a flash messages. This is shown to the user when the next view is rendered.
--
-- Will be rendered in a bootstrap alert, with the @alert-success@ styling.
-- Take a look at https://getbootstrap.com/docs/4.5/components/alerts/ for how this will look like.
--
-- This requires 'IHP.ViewSupport.renderFlashMessages' to be placed somewhere in the layout or page of the next view.
-- For example:
--
-- > myLayout view = [hsx|
-- >     {renderFlashMessages}
-- >     <main>{view}</main>
-- > |]
setSuccessMessage :: SessionVaultKey -> Request -> Text -> IO ()
setSuccessMessage vaultKey request message =
    sessionInsertText vaultKey request successMessageKey message

-- | Sets a flash messages. This is shown to the user when the next view is rendered.
--
-- Will be rendered in a bootstrap alert, with the @alert-danger@ styling.
-- Take a look at https://getbootstrap.com/docs/4.5/components/alerts/ for how this will look like.
--
-- This requires 'IHP.ViewSupport.renderFlashMessages' to be placed somewhere in the layout or page of the next view.
-- For example:
--
-- > myLayout view = [hsx|
-- >     {renderFlashMessages}
-- >     <main>{view}</main>
-- > |]
setErrorMessage :: SessionVaultKey -> Request -> Text -> IO ()
setErrorMessage vaultKey request message =
    sessionInsertText vaultKey request errorMessageKey message

-- | Returns the flash message currently set
getSuccessMessage :: SessionVaultKey -> Request -> IO (Maybe Text)
getSuccessMessage vaultKey request =
    sessionLookupText vaultKey request successMessageKey

-- | Removes the current flash message
clearSuccessMessage :: SessionVaultKey -> Request -> IO ()
clearSuccessMessage vaultKey request =
    setSuccessMessage vaultKey request ""

successMessageKey :: ByteString
successMessageKey = "flashSuccessMessage"

errorMessageKey :: ByteString
errorMessageKey = "flashErrorMessage"

consumeFlashMessagesMiddleware :: SessionVaultKey -> FlashVaultKey -> Middleware
consumeFlashMessagesMiddleware sessionVaultKey flashVaultKey next request respond =
    case Vault.lookup sessionVaultKey request.vault of
        Just (lookup, insert) -> do
            let decodeAndClear name = do
                    value <- lookup name
                    case value of
                        Just value -> do
                            insert name "" -- Clear the flash message
                            pure case Serialize.decode value of
                                Left _ -> Nothing
                                Right text -> Just text
                        Nothing -> pure Nothing


            successMessage <- decodeAndClear successMessageKey
            errorMessage <- decodeAndClear errorMessageKey

            let
                allFlashMessages = Maybe.catMaybes ((fmap SuccessFlashMessage successMessage):(fmap ErrorFlashMessage errorMessage):[])
                request' = request { vault = Vault.insert flashVaultKey allFlashMessages request.vault }

            next request' respond
        Nothing -> next request respond

requestFlashMessages :: FlashVaultKey -> Request -> Maybe [FlashMessage]
requestFlashMessages flashVaultKey request =
    Vault.lookup flashVaultKey request.vault

sessionInsertText :: SessionVaultKey -> Request -> ByteString -> Text -> IO ()
sessionInsertText vaultKey request name value =
    let insert = snd (requestSessionVault vaultKey request)
    in insert name (Serialize.encode value)

sessionLookupText :: SessionVaultKey -> Request -> ByteString -> IO (Maybe Text)
sessionLookupText vaultKey request name = do
    value <- fst (requestSessionVault vaultKey request) $ name
    pure case value of
        Just value -> case Serialize.decode value of
            Left error -> Nothing
            Right value -> Just value
        Nothing -> Nothing

requestSessionVault :: SessionVaultKey -> Request -> (ByteString -> IO (Maybe ByteString), ByteString -> ByteString -> IO ())
requestSessionVault vaultKey request =
    case Vault.lookup vaultKey request.vault of
        Just session -> session
        Nothing -> error "requestSessionVault: The session vault is missing in the request"