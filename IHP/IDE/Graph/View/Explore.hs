module IHP.IDE.Graph.View.Explore where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.SchemaCompiler as GraphQL
import qualified IHP.GraphQL.ToText as GraphQL
import IHP.IDE.Graph.View.Layout

data ExploreView
    = ExploreView
    { schema :: GraphQL.GraphQLSchema }

instance View ExploreView where
    html ExploreView { .. } = [hsx|
        <div class="d-flex flex-column h-100 w-100">
            {headerNav}
            <div id="graph-explorer-root" class="w-100 flex-grow-1" data-schema={GraphQL.toText schema}/>
        </div>
    |]