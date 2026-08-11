import React from 'react';
import TestRenderer, { act } from 'react-test-renderer';
import { renderToString } from 'react-dom/server';
import { jest } from '@jest/globals';

import { CountSubscriptionStore } from './count-subscription.js';
import { DataSubscriptionStore, dataSubscriptionKey } from './data-subscription-store.js';
import { DataSyncController } from './ihp-datasync.js';
import { query } from './ihp-querybuilder.js';
import {
    AuthCompletedContext,
    AuthCompletedProvider,
    useCount,
    useIsConnected,
    useQuery,
} from './react.js';

async function flushMicrotasks(rounds = 12) {
    for (let index = 0; index < rounds; index++) {
        await Promise.resolve();
    }
}

function installServerDouble({ queryResult = [{ id: 'task-1' }], count = 1 } = {}) {
    const controller = DataSyncController.getInstance();
    controller.sendMessage = jest.fn(async payload => {
        switch (payload.tag) {
            case 'CreateDataSubscription':
                return {
                    tag: 'DidCreateDataSubscription',
                    subscriptionId: 'query-subscription',
                    revision: 0,
                    result: queryResult,
                };
            case 'CreateCountSubscription':
                return {
                    tag: 'DidCreateCountSubscription',
                    subscriptionId: 'count-subscription',
                    count,
                };
            case 'DeleteDataSubscription':
                return { tag: 'DidDeleteDataSubscription' };
            default:
                throw new Error(`Unexpected test request: ${String(payload.tag)}`);
        }
    });
    return controller;
}

function requestTags(controller) {
    return controller.sendMessage.mock.calls.map(([payload]) => payload.tag);
}

describe('React 18 DataSync adapter integration', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
        DataSubscriptionStore.queryMap.clear();
        DataSubscriptionStore.cache.clear();
        CountSubscriptionStore.queryMap.clear();
        jest.restoreAllMocks();
    });

    test('StrictMode shares useQuery/useCount resources and cleans each up once', async () => {
        const controller = installServerDouble({ count: 7 });

        function QueryProbe() {
            const records = useQuery(query('tasks'));
            return React.createElement('span', null, records === null ? 'loading' : records.length);
        }

        function CountProbe() {
            const count = useCount(query('tasks'));
            return React.createElement('span', null, count === null ? 'loading' : count);
        }

        const strictTree = React.createElement(
            React.StrictMode,
            null,
            React.createElement(QueryProbe),
            React.createElement(QueryProbe),
            React.createElement(CountProbe),
            React.createElement(CountProbe),
        );
        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(strictTree);
            await flushMicrotasks();
        });

        expect(requestTags(controller)).toEqual([
            'CreateDataSubscription',
            'CreateCountSubscription',
        ]);
        expect(DataSubscriptionStore.queryMap.size).toBe(1);
        expect(CountSubscriptionStore.queryMap.size).toBe(1);

        // react-test-renderer does not replay effects itself. Reproduce React
        // 18's development-only release/retain cycle in one commit window.
        await act(async () => {
            renderer.unmount();
            renderer = TestRenderer.create(strictTree);
            await flushMicrotasks();
        });
        expect(requestTags(controller)).toEqual([
            'CreateDataSubscription',
            'CreateCountSubscription',
        ]);

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });

        expect(requestTags(controller)).toEqual([
            'CreateDataSubscription',
            'CreateCountSubscription',
            'DeleteDataSubscription',
            'DeleteDataSubscription',
        ]);
        expect(DataSubscriptionStore.queryMap.size).toBe(0);
        expect(CountSubscriptionStore.queryMap.size).toBe(0);
    });

    test('auth false -> true -> false gates both query and count transport work', async () => {
        const requests = [];
        const sendMessage = jest.spyOn(DataSyncController.prototype, 'sendMessage').mockImplementation(async payload => {
            requests.push(payload);
            if (payload.tag === 'CreateDataSubscription') {
                return {
                    tag: 'DidCreateDataSubscriptionV2',
                    subscriptionId: 'query-auth-scope',
                    revision: 0,
                    result: [{ id: 'task-1' }],
                };
            }
            if (payload.tag === 'CreateCountSubscription') {
                return { tag: 'DidCreateCountSubscription', subscriptionId: 'count-auth-scope', count: 3 };
            }
            return { tag: 'DidDeleteDataSubscription' };
        });
        const preAuthController = DataSyncController.getInstance();
        const preAuthSocket = { close: jest.fn(), onclose: null, onmessage: null };
        preAuthController.connection = preAuthSocket;

        function Probe() {
            const records = useQuery(query('tasks'));
            const count = useCount(query('tasks'));
            return React.createElement('span', null, `${records?.length ?? 'null'}:${count ?? 'null'}`);
        }

        const tree = authCompleted => React.createElement(
            AuthCompletedContext.Provider,
            { value: authCompleted },
            React.createElement(Probe),
        );

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(tree(false));
            await flushMicrotasks();
        });
        expect(requests).toEqual([]);
        expect(preAuthSocket.close).toHaveBeenCalledTimes(1);
        expect(preAuthController.retired).toBe(true);
        expect(DataSyncController.instance).toBeNull();
        expect(DataSubscriptionStore.queryMap.size).toBe(0);
        expect(CountSubscriptionStore.queryMap.size).toBe(0);
        expect(renderer.toJSON().children).toEqual(['null:null']);

        await act(async () => {
            renderer.update(tree(true));
            await flushMicrotasks();
        });
        expect(requests.map(payload => payload.tag)).toEqual([
            'CreateDataSubscription',
            'CreateCountSubscription',
        ]);
        const authenticatedController = DataSyncController.instance;
        expect(authenticatedController).not.toBe(preAuthController);
        expect(renderer.toJSON().children).toEqual(['1:3']);

        const authenticatedSocket = { close: jest.fn(), onclose: null, onmessage: null };
        authenticatedController.connection = authenticatedSocket;

        await act(async () => {
            renderer.update(tree(false));
            await flushMicrotasks();
        });
        expect(requests.map(payload => payload.tag)).toEqual([
            'CreateDataSubscription',
            'CreateCountSubscription',
        ]);
        expect(authenticatedSocket.close).toHaveBeenCalledTimes(1);
        expect(authenticatedController.retired).toBe(true);
        expect(DataSyncController.instance).toBeNull();
        expect(renderer.toJSON().children).toEqual(['null:null']);

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });
        sendMessage.mockRestore();
    });

    test('a cookie session handoff with no mounted query hooks creates a fresh transport for user B', async () => {
        const controllerA = installServerDouble({ queryResult: [{ id: 'user-a-task' }] });
        const socketA = { close: jest.fn(), onclose: null, onmessage: null };
        controllerA.connection = socketA;

        function Probe() {
            const records = useQuery(query('tasks'));
            return React.createElement('span', null, records?.[0]?.id ?? 'loading');
        }

        const authenticatedTree = React.createElement(
            AuthCompletedProvider,
            { value: true },
            React.createElement(Probe),
        );
        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(authenticatedTree);
            await flushMicrotasks();
        });
        expect(renderer.toJSON().children).toEqual(['user-a-task']);
        const resourceA = DataSubscriptionStore.get(query('tasks').query);

        // User A's entire application tree goes away before the session cookie
        // changes, so there are deliberately no query hooks left to perform the
        // reset on behalf of the auth layer.
        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });

        await act(async () => {
            renderer = TestRenderer.create(
                React.createElement(AuthCompletedProvider, { value: false }),
            );
            await flushMicrotasks();
        });
        expect(socketA.close).toHaveBeenCalledTimes(1);
        expect(controllerA.retired).toBe(true);
        expect(DataSyncController.instance).toBeNull();

        const resourceB = DataSubscriptionStore.get(query('tasks').query);
        expect(resourceB).not.toBe(resourceA);
        expect(DataSyncController.instance).toBeNull();

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });

        const controllerB = installServerDouble({ queryResult: [{ id: 'user-b-task' }] });
        expect(controllerB).not.toBe(controllerA);
        await act(async () => {
            renderer = TestRenderer.create(authenticatedTree);
            await flushMicrotasks();
        });

        expect(renderer.toJSON().children).toEqual(['user-b-task']);
        expect(requestTags(controllerB)).toEqual(['CreateDataSubscription']);

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });
    });

    test('a JWT change in the render-to-commit gap discards the old cache and resolves the current resource', async () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'render-a';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        const queryBuilder = query('private_tasks');
        DataSubscriptionStore.cache.set(dataSubscriptionKey(queryBuilder.query), [{ id: 'user-a-secret' }]);
        const requests = [];
        const sendMessage = jest.spyOn(DataSyncController.prototype, 'sendMessage').mockImplementation(async payload => {
            requests.push(payload);
            if (payload.tag === 'CreateDataSubscription') {
                return {
                    tag: 'DidCreateDataSubscriptionV2',
                    subscriptionId: 'render-b-subscription',
                    revision: 0,
                    result: [{ id: 'user-b-row' }],
                };
            }
            return { tag: 'DidDeleteDataSubscription' };
        });
        let switchScopeAfterFirstRender = true;

        function Probe() {
            const records = useQuery(query('private_tasks'));
            if (switchScopeAfterFirstRender) {
                switchScopeAfterFirstRender = false;
                jwt = 'render-b';
            }
            return React.createElement('span', null, records?.[0]?.id ?? 'loading');
        }

        let renderer;
        try {
            await act(async () => {
                renderer = TestRenderer.create(React.createElement(Probe));
                await flushMicrotasks();
            });

            expect(renderer.toJSON().children).toEqual(['user-b-row']);
            expect(requests.filter(payload => payload.tag === 'CreateDataSubscription')).toHaveLength(1);
            expect(DataSubscriptionStore.queryMap.size).toBe(1);
            await act(async () => {
                renderer.unmount();
                await flushMicrotasks();
            });
        } finally {
            sendMessage.mockRestore();
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });

    test('SSR returns null/false snapshots without starting a socket', () => {
        const previousWebSocket = globalThis.WebSocket;
        const webSocketConstructor = jest.fn(() => {
            throw new Error('SSR must not construct a WebSocket');
        });
        globalThis.WebSocket = webSocketConstructor;

        function Probe() {
            const records = useQuery(query('tasks'));
            const count = useCount(query('tasks'));
            const isConnected = useIsConnected();
            return React.createElement(
                'span',
                null,
                `${records === null}:${count === null}:${isConnected}`,
            );
        }

        try {
            expect(renderToString(React.createElement(Probe))).toContain('true:true:false');
            expect(DataSyncController.instance).toBeNull();
            expect(DataSubscriptionStore.queryMap.size).toBe(0);
            expect(CountSubscriptionStore.queryMap.size).toBe(0);
            expect(webSocketConstructor).not.toHaveBeenCalled();
        } finally {
            globalThis.WebSocket = previousWebSocket;
        }
    });

    test('a create failure reaches the nearest React ErrorBoundary', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn().mockRejectedValue(new Error('row policy denied'));
        const consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});

        class ErrorBoundary extends React.Component {
            constructor(props) {
                super(props);
                this.state = { error: null };
            }

            static getDerivedStateFromError(error) {
                return { error };
            }

            render() {
                return this.state.error === null
                    ? this.props.children
                    : React.createElement('span', null, this.state.error.message);
            }
        }

        function Probe() {
            useQuery(query('private_tasks'));
            return React.createElement('span', null, 'no error');
        }

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(
                React.createElement(ErrorBoundary, null, React.createElement(Probe)),
            );
            await flushMicrotasks();
        });

        expect(renderer.toJSON().children[0]).toContain('row policy denied');
        expect(requestTags(controller)).toEqual(['CreateDataSubscription']);
        expect(consoleError).toHaveBeenCalled();

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });
    });

    test('rerendering an equivalent query key does not resubscribe', async () => {
        const controller = installServerDouble();

        function Probe({ rerender }) {
            const records = useQuery(query('tasks'));
            return React.createElement('span', null, `${rerender}:${records?.length ?? 'null'}`);
        }

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(React.createElement(Probe, { rerender: 0 }));
            await flushMicrotasks();
        });

        await act(async () => {
            renderer.update(React.createElement(Probe, { rerender: 1 }));
            await flushMicrotasks();
        });

        expect(requestTags(controller)).toEqual(['CreateDataSubscription']);
        expect(DataSubscriptionStore.queryMap.size).toBe(1);
        expect(DataSubscriptionStore.queryMap.values().next().value.getSnapshot().status).toBe('live');

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });
    });

    test('useIsConnected observes a connection opened in the render-to-subscribe gap', async () => {
        const controller = DataSyncController.getInstance();
        const originalAddEventListener = controller.addEventListener.bind(controller);
        const observedSnapshots = [];
        let openWasDropped = false;

        controller.addEventListener = jest.fn((event, callback) => {
            if (event === 'open' && controller.connection === null) {
                controller.connection = {};
                openWasDropped = controller.eventListeners.open.length === 0;
            }
            originalAddEventListener(event, callback);
        });

        function Probe() {
            const isConnected = useIsConnected();
            observedSnapshots.push(isConnected);
            return React.createElement('span', null, String(isConnected));
        }

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(React.createElement(Probe));
            await flushMicrotasks();
        });

        expect(openWasDropped).toBe(true);
        expect(observedSnapshots[0]).toBe(false);
        expect(observedSnapshots.at(-1)).toBe(true);
        expect(renderer.toJSON().children).toEqual(['true']);

        await act(async () => {
            renderer.unmount();
            await flushMicrotasks();
        });
        controller.connection = null;
    });
});
