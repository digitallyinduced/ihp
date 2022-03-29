module IHP.IDE.Graph.View.Schema where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.SchemaCompiler as GraphQL
import qualified IHP.GraphQL.ToText as GraphQL
import IHP.IDE.Graph.View.Layout

data SchemaView
    = SchemaView
    { schema :: GraphQL.GraphQLSchema }

instance View SchemaView where
    html SchemaView { .. } = [hsx|
        {headerNav}
        <pre><code class="language-javascript">{GraphQL.toText schema}</code></pre>
    |]