import React, { useState, useEffect, useCallback } from 'react';
import * as ReactDOM from 'react-dom'

import { initIHPBackend, DataSyncController } from './../../../DataSync/index.js';
import { query } from './../../../DataSync/graphql.js';

import "./graph.css";
import "graphiql/graphiql.css";
import "./solarized.css";
import GraphiQL from "graphiql";
import { parse, buildASTSchema } from "graphql";
import GraphiQLExplorer from "graphiql-explorer";
import AsyncSelect, { useAsync } from "react-select/async";
import { createGraphiQLFetcher } from '@graphiql/toolkit';

function graphQLFetcher(params) {
    return query(params.query, params.variables !== null ? params.variables : {});
}

var port = (parseInt(document.location.port, 10) || 8000) - 1;
const fetcher = createGraphiQLFetcher({
    url: 'http://localhost:' + port + '/api/graphql',
});


const defaultQuery = `
{
  tasks { id }
}
`;

function App({ schema }) {
    const [query, setQuery] = useState(defaultQuery);
    const [explorerIsOpen, setExplorerOpen] = useState(true);
    const [headers, setHeaders] = useState('');

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
            fetcher={fetcher}
            defaultQuery={defaultQuery}
            editorTheme="solarized dark"
            schema={schema}
            query={query}
            onEditQuery={setQuery}
            toolbar={{
                additionalContent: <UserSelector setHeaders={setHeaders}/>
            }}
            headers={headers}
            onEditHeaders={setHeaders}
        />
    </div>
}

function userToOption(user) {
    return { value: user, label: user.email }
}

async function fetchJWTByUserId(userId) {
    return fetch('/GetJWTForUserId?userId=' + encodeURIComponent(userId)).then(res => res.text());
}

function UserSelector({ setHeaders }) {
    const [user, setUser] = useState(null);
    const [isOpen, setOpen] = useState(false);

    const setHeadersToJWT = jwt => {
        setHeaders(headersString => {
            try {
                const json = JSON.parse(headersString || '{}');
                if (jwt) {
                    json['Authorization'] = 'Bearer ' + jwt;
                } else {
                    delete json.Authorization;
                }
                return JSON.stringify(json, undefined, 4);
            } catch (e) {
                console.log('Failed to parse headers string', e);
                return headersString;
            }
        })
    };

    const onChange = useCallback(({ value }) => {
        setUser(value);
        setOpen(false);

        if (value) {
            fetchJWTByUserId(value.id).then(jwt => setHeadersToJWT(jwt));
        } else {
            setHeadersToJWT(null);
        }
    }, [setUser, setOpen]);

    const fetchOptions = async (input) => {
        const users = await fetch('/GraphUsers').then(res => res.json());
        const options = users.map(userToOption);
        options.unshift({ label: 'Logged out', value: null })

        return options;
    }

    return <div class="">
        <button class="toolbar-button" onClick={() => setOpen(!isOpen)}>Auth: {user ? user.email : 'none'}</button>
        {isOpen && <div id="user-selector">
            <AsyncSelect
                cacheOptions
                defaultOptions
                loadOptions={fetchOptions}
                autoFocus
                menuIsOpen
                placeholder="Select user"
                value={user}
                onChange={onChange}
                controlShouldRenderValue={false}
                hideSelectedOptions={false}
                isClearable={false}
            />
        </div>}
    </div>
}


$(document).on('ready turbolinks:load', function () {
    const root = document.getElementById('graph-explorer-root');
    if (!root) {
        return;
    }

    const schema = buildASTSchema(parse(root.dataset.schema));

    var port = (parseInt(document.location.port, 10) || 8000) - 1;
    initIHPBackend({ host: 'http://localhost:' + port });

    ReactDOM.render(<App schema={schema}/>, root);
});

$(document).on('turbolinks:before-render', function () {
    const root = document.getElementById('graph-explorer-root');
    if (!root) {
        return;
    }

    ReactDOM.unmountComponentAtNode(root);
});