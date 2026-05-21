module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Html)
import IHP.Controller.Context
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import qualified Data.Vault.Lazy as Vault
import Network.Wai (Request, vault)

autoRefreshMeta :: (?context :: ControllerContext, ?request :: Request) => Html
autoRefreshMeta =
    case Vault.lookup autoRefreshStateVaultKey ?request.vault of
        Just (AutoRefreshEnabled { sessionId }) -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
        _ -> mempty
