import { DataSubscription, DataSyncController, createRecord, updateRecord, deleteRecord } from './ihp-datasync.js';
import { withTransaction } from './transaction.js';
import { jest } from '@jest/globals';

function makeSubscription(records) {
    const query = {
        table: 'test',
        selectedColumns: { tag: 'SelectAll' },
        whereCondition: null,
        orderByClause: [],
        distinctOnColumn: null,
        limit: null,
        offset: null,
    };
    const sub = new DataSubscription(query);
    sub.records = records;
    sub.subscribers = [];
    return sub;
}

describe('DataSubscription.onUpdate', () => {
    test('applies appendSet normally', () => {
        const sub = makeSubscription([{ id: '1', name: 'Foo' }]);

        sub.onUpdate('1', null, { name: 'Bar' });

        expect(sub.records[0].name).toBe('FooBar');
    });

    test('skips appendSet for optimistically updated records', () => {
        const sub = makeSubscription([{ id: '1', name: 'Anrufbeantworter' }]);

        // Simulate optimistic update
        sub.onUpdate('1', { name: 'Anrufbeantworter123' }, null);
        sub.optimisticUpdatedPendingRecordIds.add('1');

        // Simulate DidUpdate arrival with appendSet
        sub.onUpdate('1', null, { name: '123' });

        expect(sub.records[0].name).toBe('Anrufbeantworter123');
    });

    test('applies appendSet for other records even with pending optimistic updates', () => {
        const sub = makeSubscription([
            { id: '1', name: 'Anrufbeantworter' },
            { id: '2', name: 'Hello' },
        ]);

        // Optimistic update only on record 1
        sub.onUpdate('1', { name: 'Anrufbeantworter123' }, null);
        sub.optimisticUpdatedPendingRecordIds.add('1');

        // appendSet for record 2 should apply normally
        sub.onUpdate('2', null, { name: ' World' });

        expect(sub.records[1].name).toBe('Hello World');
    });

    test('pending optimistic flag is cleared after processing', () => {
        const sub = makeSubscription([{ id: '1', name: 'Anrufbeantworter' }]);

        // Simulate optimistic update
        sub.onUpdate('1', { name: 'Anrufbeantworter123' }, null);
        sub.optimisticUpdatedPendingRecordIds.add('1');

        // First DidUpdate: appendSet skipped
        sub.onUpdate('1', null, { name: '123' });
        expect(sub.records[0].name).toBe('Anrufbeantworter123');

        // Subsequent append should work normally (flag cleared)
        sub.onUpdate('1', null, { name: '456' });
        expect(sub.records[0].name).toBe('Anrufbeantworter123456');
    });
});

describe('DataSyncController pending requests', () => {
    beforeEach(() => {
        jest.useFakeTimers();
        DataSyncController.instance = null;
    });

    afterEach(() => {
        jest.clearAllTimers();
        jest.useRealTimers();
    });

    test('an unrelated push message does not clear request timeouts', async () => {
        const controller = DataSyncController.getInstance();
        controller.connection = { send: jest.fn(), close: jest.fn() };

        const first = controller.sendMessage({ tag: 'FirstRequest' });
        const second = controller.sendMessage({ tag: 'SecondRequest' });
        controller.onMessage({ data: JSON.stringify({ tag: 'DidInsert', subscriptionId: 'sub', record: { id: '1' } }) });

        expect(controller.pendingRequests).toHaveLength(2);
        expect(controller.pendingRequests.every(request => request.timeout !== null)).toBe(true);

        controller.onMessage({ data: JSON.stringify({ tag: 'DataSyncResult', requestId: 0, result: [] }) });
        await expect(first).resolves.toMatchObject({ requestId: 0 });
        expect(controller.pendingRequests).toHaveLength(1);
        expect(controller.pendingRequests[0].timeout).not.toBeNull();

        controller.onClose(null);
        await expect(second).rejects.toThrow('closed before the server responded');
    });

    test('a timed out request is rejected and removed', async () => {
        const controller = DataSyncController.getInstance();
        controller.messageTimeout = 10;
        controller.connection = { send: jest.fn(), close: jest.fn() };

        const request = controller.sendMessage({ tag: 'SlowRequest' });
        jest.advanceTimersByTime(10);

        await expect(request).rejects.toThrow('timed out after 10ms');
        expect(controller.pendingRequests).toHaveLength(0);
        expect(controller.connection.close).toHaveBeenCalledTimes(1);
    });

    test('clears a failed pending connection so a later call can reconnect', async () => {
        const originalWebSocket = globalThis.WebSocket;
        const originalLocation = globalThis.location;
        const originalDocument = globalThis.document;
        class FailingWebSocket {
            constructor() {
                queueMicrotask(() => this.onerror(new Event('error')));
            }
        }
        class SuccessfulWebSocket {
            send = jest.fn();
            close = jest.fn();

            constructor() {
                queueMicrotask(() => this.onopen(new Event('open')));
            }
        }

        try {
            const controller = DataSyncController.getInstance();
            controller.connectionRetryLimit = 1;
            globalThis.location = { protocol: 'http:' };
            globalThis.document = { location: { hostname: 'localhost', port: '8000' } };
            globalThis.WebSocket = FailingWebSocket;

            const failedConnection = controller.startConnection();
            jest.runAllTicks();
            await expect(failedConnection).rejects.toBeInstanceOf(Event);
            expect(controller.pendingConnection).toBeNull();

            globalThis.WebSocket = SuccessfulWebSocket;
            const successfulConnection = controller.startConnection();
            jest.runAllTicks();
            await expect(successfulConnection).resolves.toBeInstanceOf(SuccessfulWebSocket);
            expect(controller.connection).toBeInstanceOf(SuccessfulWebSocket);
        } finally {
            if (originalWebSocket === undefined) {
                delete globalThis.WebSocket;
            } else {
                globalThis.WebSocket = originalWebSocket;
            }
            if (originalLocation === undefined) {
                delete globalThis.location;
            } else {
                globalThis.location = originalLocation;
            }
            if (originalDocument === undefined) {
                delete globalThis.document;
            } else {
                globalThis.document = originalDocument;
            }
        }
    });

    test('times out a WebSocket connection attempt that never settles', async () => {
        const originalWebSocket = globalThis.WebSocket;
        const originalLocation = globalThis.location;
        const originalDocument = globalThis.document;
        let socket;
        class HangingWebSocket {
            close = jest.fn();

            constructor() {
                socket = this;
            }
        }

        try {
            const controller = DataSyncController.getInstance();
            controller.connectionAttemptTimeout = 10;
            controller.connectionRetryLimit = 1;
            globalThis.location = { protocol: 'http:' };
            globalThis.document = { location: { hostname: 'localhost', port: '8000' } };
            globalThis.WebSocket = HangingWebSocket;

            const request = controller.sendMessage({ tag: 'NeverConnects' });
            jest.advanceTimersByTime(10);

            await expect(request).rejects.toThrow('connection attempt timed out after 10ms');
            expect(socket.close).toHaveBeenCalledTimes(1);
            expect(controller.pendingConnection).toBeNull();
            expect(controller.pendingRequests).toHaveLength(0);
            expect(controller.outbox).toHaveLength(0);
        } finally {
            if (originalWebSocket === undefined) {
                delete globalThis.WebSocket;
            } else {
                globalThis.WebSocket = originalWebSocket;
            }
            if (originalLocation === undefined) {
                delete globalThis.location;
            } else {
                globalThis.location = originalLocation;
            }
            if (originalDocument === undefined) {
                delete globalThis.document;
            } else {
                globalThis.document = originalDocument;
            }
        }
    });
});

describe('Optimistic CRUD coordination', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
    });

    test('a child waits for the referenced parent create, not for its own response', async () => {
        const controller = DataSyncController.getInstance();
        let resolveParent;
        const sentIds = [];
        controller.sendMessage = jest.fn(payload => {
            sentIds.push(payload.record.id);
            if (payload.record.id === 'parent') {
                return new Promise(resolve => { resolveParent = resolve; });
            }
            return Promise.resolve({ tag: 'DidCreateRecord', record: payload.record });
        });

        const parent = createRecord('parents', { id: 'parent', name: 'Parent' });
        await Promise.resolve();
        const child = createRecord('children', { id: 'child', parentId: 'parent' });
        await Promise.resolve();

        expect(sentIds).toEqual(['parent']);
        resolveParent({ tag: 'DidCreateRecord', record: { id: 'parent', name: 'Parent' } });
        await parent;
        await child;

        expect(sentIds).toEqual(['parent', 'child']);
    });

    test('transaction rollback leaves subscriptions unchanged', async () => {
        const controller = DataSyncController.getInstance();
        const subscription = makeSubscription([]);
        controller.dataSubscriptions.push(subscription);
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'StartTransaction') {
                return { tag: 'DidStartTransaction', transactionId: 'tx' };
            }
            if (payload.tag === 'CreateRecordMessage') {
                return { tag: 'DidCreateRecord', record: payload.record };
            }
            return { tag: 'DidRollbackTransaction', transactionId: 'tx' };
        });

        await expect(withTransaction(async transaction => {
            await transaction.createRecord('test', { id: 'ghost', title: 'Ghost' });
            throw new Error('abort');
        })).rejects.toThrow('abort');

        expect(subscription.records).toEqual([]);
        expect(subscription.optimisticCreatedPendingRecordIds).toEqual([]);
    });

    test('transactional updates and deletes are not applied optimistically', async () => {
        const controller = DataSyncController.getInstance();
        const originalRecord = { id: 'record', title: 'Original' };
        const subscription = makeSubscription([originalRecord]);
        controller.dataSubscriptions.push(subscription);
        controller.sendMessage = jest.fn(async payload => {
            switch (payload.tag) {
                case 'StartTransaction': return { tag: 'DidStartTransaction', transactionId: 'tx' };
                case 'UpdateRecordMessage': return { tag: 'DidUpdateRecord', record: { id: payload.id, ...payload.patch } };
                case 'DeleteRecordMessage': return { tag: 'DidDeleteRecord' };
                default: return { tag: 'DidRollbackTransaction', transactionId: 'tx' };
            }
        });

        await expect(withTransaction(async transaction => {
            await transaction.updateRecord('test', 'record', { title: 'Changed' });
            await transaction.deleteRecord('test', 'record');
            throw new Error('abort');
        })).rejects.toThrow('abort');

        expect(subscription.records).toEqual([originalRecord]);
    });

    test('does not refresh a limited query before an optimistic create reaches the server', async () => {
        const controller = DataSyncController.getInstance();
        const subscription = makeSubscription([
            { id: 'a', title: 'A' },
            { id: 'c', title: 'C' },
        ]);
        subscription.query.orderByClause = [{ orderByColumn: 'title', orderByDirection: 'Asc' }];
        subscription.query.limit = 2;
        controller.dataSubscriptions.push(subscription);
        let resolveCreate;
        controller.sendMessage = jest.fn(payload => {
            if (payload.tag === 'CreateRecordMessage') {
                return new Promise(resolve => { resolveCreate = resolve; });
            }
            return Promise.resolve({ tag: 'DataSyncResult', result: [
                { id: 'a', title: 'A' },
                { id: 'b', title: 'B' },
            ] });
        });

        const create = createRecord('test', { id: 'b', title: 'B' });
        await Promise.resolve();
        await Promise.resolve();

        expect(subscription.records.map(record => record.id)).toEqual(['a', 'b', 'c']);
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['CreateRecordMessage']);

        resolveCreate({ tag: 'DidCreateRecord', record: { id: 'b', title: 'B' } });
        await create;
        subscription.onCreate({ id: 'b', title: 'B' });

        expect(subscription.records.map(record => record.id)).toEqual(['a', 'b']);
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateRecordMessage',
            'DataSyncQuery',
        ]);
    });

    test('restores the complete limited window when an optimistic create fails offline', async () => {
        const controller = DataSyncController.getInstance();
        const subscription = makeSubscription([
            { id: 'a', title: 'A' },
            { id: 'c', title: 'C' },
        ]);
        subscription.query.orderByClause = [{ orderByColumn: 'title', orderByDirection: 'Asc' }];
        subscription.query.limit = 2;
        controller.dataSubscriptions.push(subscription);
        controller.sendMessage = jest.fn(async payload => {
            throw new Error(`${payload.tag} failed`);
        });
        const consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});

        try {
            await expect(createRecord('test', { id: 'b', title: 'B' })).rejects.toThrow('CreateRecordMessage failed');
            await Promise.resolve();

            expect(subscription.records.map(record => record.id)).toEqual(['a', 'c']);
            expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
                'CreateRecordMessage',
                'DataSyncQuery',
            ]);
        } finally {
            consoleError.mockRestore();
        }
    });

    test('does not refresh limited queries before optimistic updates and deletes are sent', async () => {
        const controller = DataSyncController.getInstance();
        const subscription = makeSubscription([
            { id: 'a', title: 'A' },
            { id: 'b', title: 'B' },
        ]);
        subscription.query.orderByClause = [{ orderByColumn: 'title', orderByDirection: 'Asc' }];
        subscription.query.limit = 2;
        controller.dataSubscriptions.push(subscription);
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'UpdateRecordMessage') {
                return { tag: 'DidUpdateRecord', record: { id: payload.id, ...payload.patch } };
            }
            return { tag: 'DidDeleteRecord' };
        });

        await updateRecord('test', 'a', { title: 'Z' });
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['UpdateRecordMessage']);

        controller.sendMessage.mockClear();
        await deleteRecord('test', 'b');
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['DeleteRecordMessage']);
    });
});

describe('DataSubscription query semantics', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
    });

    test('keeps records ordered after inserts and updates', () => {
        const subscription = makeSubscription([
            { id: 'a', title: 'A' },
            { id: 'c', title: 'C' },
        ]);
        subscription.query.orderByClause = [{ orderByColumn: 'title', orderByDirection: 'Asc' }];

        subscription.onCreate({ id: 'b', title: 'B' });
        expect(subscription.records.map(record => record.id)).toEqual(['a', 'b', 'c']);

        subscription.onUpdate('c', { title: '0' }, null);
        expect(subscription.records.map(record => record.id)).toEqual(['c', 'a', 'b']);
    });

    test('enforces limit immediately and refreshes the exact server window', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async () => ({
            tag: 'DataSyncResult',
            result: [{ id: 'a', title: 'A' }, { id: 'b', title: 'B' }]
        }));
        const subscription = makeSubscription([
            { id: 'a', title: 'A' },
            { id: 'c', title: 'C' },
        ]);
        subscription.query.orderByClause = [{ orderByColumn: 'title', orderByDirection: 'Asc' }];
        subscription.query.limit = 2;

        subscription.onCreate({ id: 'b', title: 'B' });
        expect(subscription.records.map(record => record.id)).toEqual(['a', 'b']);
        await Promise.resolve();
        expect(controller.sendMessage).toHaveBeenCalledWith(expect.objectContaining({ tag: 'DataSyncQuery' }));
    });

    test('subscription errors notify subscribers', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn().mockRejectedValue(new Error('denied'));
        const subscription = makeSubscription([]);
        const subscriber = jest.fn();
        subscription.subscribers = [subscriber];
        const createPromise = subscription.createOnServer();
        const internalPromise = subscription.createOnServerPromise.catch(() => {});

        await expect(createPromise).rejects.toThrow('denied while trying to subscribe');
        await internalPromise;
        expect(subscription.connectError).toBeInstanceOf(Error);
        expect(subscriber).toHaveBeenCalled();
    });
});

describe('DataSubscription disconnect cleanup', () => {
    beforeEach(() => {
        jest.useFakeTimers();
        DataSyncController.instance = null;
    });

    afterEach(() => {
        jest.clearAllTimers();
        jest.useRealTimers();
    });

    test('removes an unused subscription locally after its socket was closed', async () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        sub.isClosed = true;
        controller.dataSubscriptions.push(sub);

        await sub.close();

        expect(controller.dataSubscriptions).not.toContain(sub);
        expect(sub.isConnected).toBe(false);
    });

    test('prunes an unused subscription after the React commit grace period', async () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        controller.dataSubscriptions.push(sub);

        sub.onDataSyncClosed();

        jest.advanceTimersByTime(999);
        expect(controller.dataSubscriptions).toContain(sub);

        jest.advanceTimersByTime(1);
        await Promise.resolve();
        expect(controller.dataSubscriptions).not.toContain(sub);
    });

    test('keeps a subscription when React commits before reconnect', () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        controller.dataSubscriptions.push(sub);

        sub.onDataSyncClosed();
        jest.advanceTimersByTime(100);
        const unsubscribe = sub.subscribe(() => {});
        jest.advanceTimersByTime(900);

        expect(controller.dataSubscriptions).toContain(sub);
        expect(sub.subscribers).toHaveLength(1);

        unsubscribe();
    });

    test('notifies local stores only once when close is repeated', async () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        const replacement = makeSubscription([]);
        const store = new Map([['test', sub]]);
        let closeNotifications = 0;
        sub.isClosed = true;
        sub.onClose = () => {
            closeNotifications++;
            store.delete('test');
        };
        controller.dataSubscriptions.push(sub);

        await sub.close();
        store.set('test', replacement);
        await sub.close();

        expect(closeNotifications).toBe(1);
        expect(store.get('test')).toBe(replacement);
    });

    test('deletes a reconnect response that arrives after the subscription was closed', async () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        const sentMessages = [];
        let resolveCreateOnServer;
        sub.isClosed = true;
        controller.dataSubscriptions.push(sub);
        controller.sendMessage = (message) => {
            sentMessages.push(message);
            if (message.tag === 'CreateDataSubscription') {
                return new Promise(resolve => { resolveCreateOnServer = resolve; });
            }
            return Promise.resolve({});
        };

        const reconnect = sub.onDataSyncReconnect();
        await sub.close();
        resolveCreateOnServer({ subscriptionId: 'reconnected-id', result: [] });
        await reconnect;

        expect(sentMessages).toEqual([
            { tag: 'CreateDataSubscription', query: sub.query },
            { tag: 'DeleteDataSubscription', subscriptionId: 'reconnected-id' },
        ]);
        expect(controller.dataSubscriptions).not.toContain(sub);
        expect(sub.isClosed).toBe(true);
        expect(sub.isConnected).toBe(false);
        expect(sub.subscriptionId).toBe(null);
    });
});
