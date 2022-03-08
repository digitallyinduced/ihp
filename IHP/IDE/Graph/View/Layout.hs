module IHP.IDE.Graph.View.Layout
( headerNav
) where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes
import qualified Data.Text as Text
import IHP.IDE.ToolServer.Helper.View

headerNav :: Html
headerNav = [hsx|
    <div class="view-selector pb-0">
        <div class="container-fluid d-flex">
            <a href={ExploreAction} class={classes [("active", exploreActive)]} style="padding-bottom: 6px !important">
                Explore
            </a>
            
            <a href={SchemaAction} class={classes [("active", schemaActive)]} style="padding-bottom: 6px !important">
                Schema
            </a>

            <div class="d-inline-block ml-auto">
                <div
                    style="cursor: pointer"
                    class="text-muted"
                    data-clipboard-text={url}
                    title="Copied!"
                    onclick="$(this).tooltip({title: 'Copied!', placement: 'bottom', trigger: 'manual', container: 'body'}).tooltip('show'); setTimeout(() => $(this).tooltip('hide'), 2000)"
                >
                    {url}
                </div>
            </div>
        </div>
    </div>
|]
    where
        exploreActive :: Bool
        exploreActive = isActivePath ExploreAction

        schemaActive :: Bool
        schemaActive = isActivePath SchemaAction

        url :: Text
        url = "http://localhost:8000/api/graphql"