module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.HSX.MarkupQQ (hsx)
import IHP.HSX.Markup (Markup)
import IHP.Controller.Context
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import qualified Data.Vault.Lazy as Vault
import Network.Wai (vault)

autoRefreshMeta :: (?context :: ControllerContext) => Markup
autoRefreshMeta =
    case Vault.lookup autoRefreshStateVaultKey ?context.request.vault of
        Just (AutoRefreshEnabled { sessionId }) -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
        _ -> mempty
