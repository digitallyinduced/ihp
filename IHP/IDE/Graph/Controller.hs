module IHP.IDE.Graph.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.Graph.View.Explore
import IHP.IDE.Graph.View.Schema

import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import qualified IHP.GraphQL.ToText as GraphQL
import qualified IHP.GraphQL.SchemaCompiler as GraphQL

instance Controller GraphController where
    action ExploreAction = do
        SchemaDesigner.parseSchemaSql >>= \case
            Left parserError -> fail (cs parserError)
            Right sqlSchema -> do
                let schema = GraphQL.sqlSchemaToGraphQLSchema sqlSchema
                render ExploreView { .. }

    action SchemaAction = do
        SchemaDesigner.parseSchemaSql >>= \case
            Left parserError -> fail (cs parserError)
            Right sqlSchema -> do
                let schema = GraphQL.sqlSchemaToGraphQLSchema sqlSchema
                render SchemaView { .. }
