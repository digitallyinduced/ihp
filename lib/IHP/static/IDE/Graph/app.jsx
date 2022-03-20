import React, { useState, useEffect } from 'react';
import * as ReactDOM from 'react-dom'

import { initIHPBackend, DataSyncController } from './../../../DataSync/index.js';
import { query } from './../../../DataSync/graphql.js';

import "./graph.css";
import "graphiql/graphiql.css";
import "./solarized.css";
import GraphiQL from "graphiql";
import { parse, buildASTSchema } from "graphql";
import GraphiQLExplorer from "graphiql-explorer";

function graphQLFetcher(params) {
    return query(params.query, params.variables !== null ? params.variables : {});
}

const defaultQuery = `
{
  tasks { id }
}
`;

function App({ schema }) {
    const [query, setQuery] = useState(defaultQuery);
    const [explorerIsOpen, setExplorerOpen] = useState(true);

    return <div id="graphiql-container" className="d-flex h-100">
        <GraphiQLExplorer
            schema={schema}
            query={query}
            onEdit={setQuery}
            onRunOperation={operationName =>
            this._graphiql.handleRunQuery(operationName)
            }
            explorerIsOpen={explorerIsOpen}
            onToggleExplorer={setExplorerOpen}
            colors={{
                keyword: '#B11A04',
                // OperationName, FragmentName
                def: '#D2054E',
                // FieldName
                property: 'hsla(196, 13%, 80%, 0.8)',
                // FieldAlias
                qualifier: '#1C92A9',
                // ArgumentName and ObjectFieldName
                attribute: '#8B2BB9',
                number: '#2882F9',
                string: '#D64292',
                // Boolean
                builtin: '#D47509',
                // Enum
                string2: '#0B7FC7',
                variable: '#397D13',
                // Type
                atom: '#CA9800',
            }}
            checkboxChecked={
                <div class="custom-control custom-checkbox">
                    <input id="allowNull" type="checkbox" class="mr-1 custom-control-input" checked/>
                    <label class="custom-control-label"></label>
                </div>
            }
            checkboxUnchecked={
                <div class="custom-control custom-checkbox">
                    <input id="allowNull" type="checkbox" class="mr-1 custom-control-input"/>
                    <label class="custom-control-label"></label>
                </div>
            }
        />
        <GraphiQL
            fetcher={graphQLFetcher}
            defaultQuery={defaultQuery}
            editorTheme="solarized dark"
            schema={schema}
            query={query}
            onEditQuery={setQuery}
        />
    </div>
}

$(document).on('ready turbolinks:load', function () {
    const root = document.getElementById('graph-explorer-root');
    const schema = buildASTSchema(parse(root.dataset.schema));
    console.log('Schema', schema)

    var port = (parseInt(document.location.port, 10) || 8000) - 1;
    initIHPBackend({ host: 'http://localhost:' + port });
    ReactDOM.render(<App schema={schema}/>, root);
});
