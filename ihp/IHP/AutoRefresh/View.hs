module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import qualified Data.Vault.Lazy as Vault
import Network.Wai (vault)

autoRefreshMeta :: (?context :: ControllerContext) => Html5.Html
autoRefreshMeta =
    case Vault.lookup autoRefreshStateVaultKey (vault ?context.request) of
        Just (AutoRefreshEnabled { sessionId }) -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
        _ -> mempty
