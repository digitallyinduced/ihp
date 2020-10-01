module IHP.AutoRefresh.View where

import IHP.Prelude
import IHP.ViewSupport
import IHP.AutoRefresh.Types
import IHP.HtmlSupport.QQ (hsx)
import qualified Text.Blaze.Html5 as Html5

autoRefreshMeta :: (?viewContext :: viewContext, HasField "autoRefreshState" viewContext AutoRefreshState) => Html5.Html
autoRefreshMeta = case ?viewContext |> get #autoRefreshState of
        AutoRefreshDisabled -> mempty
        AutoRefreshEnabled { sessionId } ->  [hsx|<meta property="ihp-auto-refresh-id" content={tshow sessionId}/>|]