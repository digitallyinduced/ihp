import React from 'react';
import TestRenderer from 'react-test-renderer';
import { jest } from '@jest/globals';
import { DataSyncController } from './ihp-datasync.js';
import { query } from './ihp-querybuilder.js';
import { DataSubscriptionStore, useQuery } from './react.js';

const { act } = TestRenderer;

function deferred() {
    let resolve;
    let reject;
    const promise = new Promise((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    return { promise, resolve, reject };
}

async function flushPromises() {
    await Promise.resolve();
    await Promise.resolve();
}

function QueryProbe({ table, onRender }) {
    const records = useQuery(query(table));
    onRender(records);
    return null;
}

async function unmount(renderer) {
    await act(async () => {
        renderer.unmount();
        await flushPromises();
    });
    await act(async () => {
        jest.runOnlyPendingTimers();
        await flushPromises();
    });
}

describe('useQuery lifecycle', () => {
    beforeEach(() => {
        jest.useFakeTimers();
        DataSyncController.instance = null;
        DataSubscriptionStore.cache.clear();
        DataSubscriptionStore.queryMap.clear();
    });

    afterEach(() => {
        jest.clearAllTimers();
        jest.useRealTimers();
    });

    test('deduplicates identical hooks and survives the Strict Mode effect cycle', async () => {
        const controller = DataSyncController.getInstance();
        const sentMessages = [];
        controller.sendMessage = jest.fn((message) => {
            sentMessages.push(message);
            if (message.tag === 'CreateDataSubscription') {
                return Promise.resolve({ subscriptionId: 'shared-subscription', result: [{ id: '1', title: 'Shared' }] });
            }
            return Promise.resolve({});
        });
        const firstResults = [];
        const secondResults = [];
        const app = React.createElement(
            React.StrictMode,
            null,
            React.createElement(QueryProbe, { table: 'shared_records', onRender: records => firstResults.push(records) }),
            React.createElement(QueryProbe, { table: 'shared_records', onRender: records => secondResults.push(records) }),
        );

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(app, { unstable_strictMode: true });
            await flushPromises();
        });

        expect(sentMessages.filter(message => message.tag === 'CreateDataSubscription')).toHaveLength(1);
        expect(controller.getOptimisticDataSubscriptions()).toHaveLength(1);
        expect(firstResults.at(-1)).toEqual([{ id: '1', title: 'Shared' }]);
        expect(secondResults.at(-1)).toEqual([{ id: '1', title: 'Shared' }]);

        await unmount(renderer);

        expect(sentMessages.filter(message => message.tag === 'DeleteDataSubscription')).toEqual([
            { tag: 'DeleteDataSubscription', subscriptionId: 'shared-subscription' },
        ]);
        expect(controller.getOptimisticDataSubscriptions()).toHaveLength(0);
    });

    test('reuses a subscription when React releases and reacquires it in one commit', async () => {
        const controller = DataSyncController.getInstance();
        const sentMessages = [];
        controller.sendMessage = jest.fn((message) => {
            sentMessages.push(message);
            if (message.tag === 'CreateDataSubscription') {
                return Promise.resolve({ subscriptionId: 'remounted-subscription', result: [] });
            }
            return Promise.resolve({});
        });
        const renderProbe = key => React.createElement(
            React.StrictMode,
            null,
            React.createElement(QueryProbe, { key, table: 'remounted_records', onRender: () => {} }),
        );

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(renderProbe('first'));
            await flushPromises();
        });
        await act(async () => {
            renderer.update(renderProbe('second'));
            await flushPromises();
        });

        expect(sentMessages.filter(message => message.tag === 'CreateDataSubscription')).toHaveLength(1);
        expect(sentMessages.filter(message => message.tag === 'DeleteDataSubscription')).toHaveLength(0);

        await unmount(renderer);

        expect(sentMessages.filter(message => message.tag === 'DeleteDataSubscription')).toEqual([
            { tag: 'DeleteDataSubscription', subscriptionId: 'remounted-subscription' },
        ]);
    });

    test('deletes a stale response after a rapid query change', async () => {
        const controller = DataSyncController.getInstance();
        const createRequests = [];
        const sentMessages = [];
        controller.sendMessage = jest.fn((message) => {
            sentMessages.push(message);
            if (message.tag === 'CreateDataSubscription') {
                const response = deferred();
                createRequests.push({ query: message.query, response });
                return response.promise;
            }
            return Promise.resolve({});
        });
        const results = [];

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(React.createElement(QueryProbe, { table: 'first_records', onRender: records => results.push(records) }));
            await flushPromises();
        });
        await act(async () => {
            renderer.update(React.createElement(QueryProbe, { table: 'second_records', onRender: records => results.push(records) }));
            await flushPromises();
        });
        await act(async () => {
            jest.runOnlyPendingTimers();
            await flushPromises();
        });

        expect(createRequests).toHaveLength(2);

        await act(async () => {
            createRequests[0].response.resolve({ subscriptionId: 'stale-subscription', result: [{ id: 'stale' }] });
            await flushPromises();
        });
        await act(async () => {
            createRequests[1].response.resolve({ subscriptionId: 'current-subscription', result: [{ id: 'current' }] });
            await flushPromises();
        });

        expect(results).not.toContainEqual([{ id: 'stale' }]);
        expect(results.at(-1)).toEqual([{ id: 'current' }]);
        expect(sentMessages).toContainEqual({ tag: 'DeleteDataSubscription', subscriptionId: 'stale-subscription' });

        await unmount(renderer);
    });

    test('deletes a reconnect response that arrives after unmount', async () => {
        const controller = DataSyncController.getInstance();
        const reconnectResponse = deferred();
        const sentMessages = [];
        let createCount = 0;
        controller.sendMessage = jest.fn((message) => {
            sentMessages.push(message);
            if (message.tag === 'CreateDataSubscription') {
                createCount++;
                if (createCount === 1) {
                    return Promise.resolve({ subscriptionId: 'initial-subscription', result: [] });
                }
                return reconnectResponse.promise;
            }
            return Promise.resolve({});
        });

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(React.createElement(QueryProbe, { table: 'reconnect_records', onRender: () => {} }));
            await flushPromises();
        });
        await act(async () => {
            for (const listener of controller.eventListeners.close.slice()) {
                listener(null);
            }
            for (const listener of controller.eventListeners.reconnect.slice()) {
                listener();
            }
            await flushPromises();
        });

        await unmount(renderer);
        await act(async () => {
            reconnectResponse.resolve({ subscriptionId: 'stale-reconnect-subscription', result: [] });
            await flushPromises();
        });

        expect(sentMessages).toContainEqual({ tag: 'DeleteDataSubscription', subscriptionId: 'stale-reconnect-subscription' });
        expect(controller.getOptimisticDataSubscriptions()).toHaveLength(0);
    });

    test('publishes optimistic create, update and delete changes', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn((message) => {
            if (message.tag === 'CreateDataSubscription') {
                return Promise.resolve({
                    subscriptionId: 'optimistic-subscription',
                    result: [{ id: '1', title: 'Before' }],
                });
            }
            return Promise.resolve({});
        });
        const results = [];

        let renderer;
        await act(async () => {
            renderer = TestRenderer.create(React.createElement(QueryProbe, { table: 'optimistic_records', onRender: records => results.push(records) }));
            await flushPromises();
        });
        const optimisticSubscription = controller.getOptimisticDataSubscriptions()[0];

        await act(async () => {
            optimisticSubscription.onCreateOptimistic({ id: '2', title: 'Draft' });
        });
        expect(results.at(-1)).toEqual([
            { id: '1', title: 'Before' },
            { id: '2', title: 'Draft' },
        ]);

        await act(async () => {
            for (const listener of controller.eventListeners.message.slice()) {
                listener({
                    tag: 'DidInsert',
                    subscriptionId: 'optimistic-subscription',
                    record: { id: '2', title: 'Created' },
                });
            }
        });
        expect(results.at(-1)).toEqual([
            { id: '1', title: 'Before' },
            { id: '2', title: 'Created' },
        ]);

        await act(async () => {
            optimisticSubscription.onUpdate('1', { title: 'Before!' }, null);
            optimisticSubscription.optimisticUpdatedPendingRecordIds.add('1');
            for (const listener of controller.eventListeners.message.slice()) {
                listener({
                    tag: 'DidUpdate',
                    subscriptionId: 'optimistic-subscription',
                    id: '1',
                    changeSet: null,
                    appendSet: { title: '!' },
                });
            }
        });
        expect(results.at(-1)[0]).toEqual({ id: '1', title: 'Before!' });

        await act(async () => {
            for (const listener of controller.eventListeners.message.slice()) {
                listener({ tag: 'DidDelete', subscriptionId: 'optimistic-subscription', id: '2' });
            }
        });
        expect(results.at(-1)).toEqual([{ id: '1', title: 'Before!' }]);

        await unmount(renderer);
    });
});
