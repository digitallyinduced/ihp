import { DataSubscription, DataSyncController } from './ihp-datasync.js';
import { jest } from '@jest/globals';

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
});
