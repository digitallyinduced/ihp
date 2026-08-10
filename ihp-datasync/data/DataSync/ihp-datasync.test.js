import { DataSubscription, DataSyncController } from './ihp-datasync.js';

function makeSubscription(records) {
    const query = {
        table: 'test',
        conditionExpression: [],
        orderByClause: [],
        distinctOnColumn: null,
        limit: null,
        offset: null,
    };
    const sub = new DataSubscription(query);
    sub.records = records;
    sub.subscribers = [];
    sub.updateSubscribers = function () {};
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

describe('DataSubscription disconnect cleanup', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
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

    test('keeps an imperative subscription registered for reconnect after disconnect', () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        controller.dataSubscriptions.push(sub);

        sub.onDataSyncClosed();

        expect(controller.dataSubscriptions).toContain(sub);
        expect(sub.isClosed).toBe(true);
        expect(sub.isConnected).toBe(false);
    });

    test('removes a disconnected subscription after its last subscriber leaves', () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        const unsubscribe = sub.subscribe(() => {});
        controller.dataSubscriptions.push(sub);

        sub.onDataSyncClosed();
        unsubscribe();

        expect(controller.dataSubscriptions).not.toContain(sub);
        expect(sub.subscribers).toHaveLength(0);
    });

    test('deletes a connected subscription after its last subscriber leaves', async () => {
        const controller = DataSyncController.getInstance();
        const sub = makeSubscription([]);
        const sentMessages = [];
        controller.sendMessage = (message) => {
            sentMessages.push(message);
            return Promise.resolve({});
        };
        sub.isConnected = true;
        sub.subscriptionId = 'subscription-id';
        controller.dataSubscriptions.push(sub);
        const unsubscribe = sub.subscribe(() => {});

        unsubscribe();
        await Promise.resolve();

        expect(sentMessages).toEqual([
            { tag: 'DeleteDataSubscription', subscriptionId: 'subscription-id' },
        ]);
        expect(controller.dataSubscriptions).not.toContain(sub);
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
