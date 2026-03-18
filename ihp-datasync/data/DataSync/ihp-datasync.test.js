import { DataSubscription } from './ihp-datasync.js';

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
