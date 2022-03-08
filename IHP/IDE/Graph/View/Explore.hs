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
        {headerNav}
        <div id="graph-explorer-root" class="h-100" data-schema={GraphQL.toText schema}/>
    |]