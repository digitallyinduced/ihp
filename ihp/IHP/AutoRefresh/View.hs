module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.AutoRefresh.Types
import IHP.HSX.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5
import IHP.Controller.Context
import IHP.AutoRefresh (autoRefreshStateVaultKey)
import qualified Data.Vault.Lazy as Vault

autoRefreshMeta :: (?context :: ControllerContext) => Html5.Html
autoRefreshMeta =
    case Vault.lookup autoRefreshStateVaultKey ?context.request.vault <|> autoRefreshStateFromContext of
        Just (AutoRefreshEnabled { sessionId }) -> case autoRefreshTargetFromContext of
            Just target -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId} data-ihp-auto-refresh-target={target}/>|]
            Nothing -> [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]
        _ -> mempty
    where
        autoRefreshStateFromContext = case ?context of
            FrozenControllerContext {} -> maybeFromFrozenContext @AutoRefreshState
            ControllerContext {} -> Nothing

        autoRefreshTargetFromContext = case ?context of
            FrozenControllerContext {} ->
                maybeFromFrozenContext @AutoRefreshTarget
                    |> fmap (\(AutoRefreshTarget target) -> target)
            ControllerContext {} -> Nothing
