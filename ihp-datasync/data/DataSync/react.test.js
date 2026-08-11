import { CountSubscription, CountSubscriptionStore } from './count-subscription.js';
import { dataSubscriptionKey } from './data-subscription-store.js';
import { DataSyncController } from './ihp-datasync.js';
import { jest } from '@jest/globals';

const countQuery = {
    table: 'tasks',
    selectedColumns: { tag: 'SelectAll' },
    whereCondition: null,
    orderByClause: [],
    distinctOnColumn: null,
    limit: null,
    offset: null,
};

async function flushMicrotasks(rounds = 8) {
    for (let index = 0; index < rounds; index++) {
        await Promise.resolve();
    }
}

describe('CountSubscription external store', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
        CountSubscriptionStore.queryMap.clear();
    });

    test('constructing and looking up count resources does not create a controller', () => {
        const direct = new CountSubscription(countQuery);
        const stored = CountSubscriptionStore.get({ ...countQuery, table: 'other_tasks' });

        expect(DataSyncController.instance).toBeNull();
        expect(direct.getSnapshot().status).toBe('idle');
        expect(stored.getSnapshot().status).toBe('idle');
    });

    test('lookup is inert and equal queries share one resource', () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn();

        const first = CountSubscriptionStore.get(countQuery);
        const second = CountSubscriptionStore.get({ ...countQuery });

        expect(first).toBe(second);
        expect(controller.sendMessage).not.toHaveBeenCalled();
        expect(first.getSnapshot()).toBe(first.getSnapshot());
    });

    test('mutating the public count query cannot alter the transport query', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateCountSubscription'
            ? { subscriptionId: 'stable-count-query', count: 0 }
            : {});
        const resource = new CountSubscription(countQuery);
        resource.query.limit = 1;
        resource.query.orderByClause.push({ orderByColumn: 'createdAt', orderByDirection: 'Desc' });

        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        const transportedQuery = controller.sendMessage.mock.calls[0][0].query;
        expect(transportedQuery.limit).toBeNull();
        expect(transportedQuery.orderByClause).toEqual([]);
        expect(Object.isFrozen(transportedQuery)).toBe(true);

        release();
        await flushMicrotasks();
    });

    test('StrictMode-style release and retain creates once and final release deletes once', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateCountSubscription'
            ? { subscriptionId: 'count', count: 4 }
            : {});
        const resource = CountSubscriptionStore.get(countQuery);

        const firstRelease = resource.subscribe(() => {});
        firstRelease();
        const finalRelease = resource.subscribe(() => {});
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['CreateCountSubscription']);

        finalRelease();
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateCountSubscription',
            'DeleteDataSubscription',
        ]);
        expect(CountSubscriptionStore.queryMap.size).toBe(0);
    });

    test('clears the count while reconnecting and recreates exactly once', async () => {
        const controller = DataSyncController.getInstance();
        let createCount = 0;
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'CreateCountSubscription') {
                createCount++;
                return { subscriptionId: `count-${createCount}`, count: createCount * 10 };
            }
            return {};
        });
        const resource = new CountSubscription(countQuery);
        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        expect(resource.getCount()).toBe(10);

        for (const listener of [...controller.eventListeners.close]) listener(null);
        expect(resource.getCount()).toBeNull();
        expect(resource.getSnapshot().status).toBe('reconnecting');

        for (const listener of [...controller.eventListeners.reconnect]) listener();
        for (const listener of [...controller.eventListeners.reconnect]) listener();
        await flushMicrotasks();
        expect(createCount).toBe(2);
        expect(resource.getCount()).toBe(20);

        for (const listener of [...controller.eventListeners.message]) {
            listener({ tag: 'DidChangeCount', subscriptionId: 'count-1', count: 99 });
        }
        expect(resource.getCount()).toBe(20);
        release();
        await flushMicrotasks();
    });

    test('legacy count subscribers are isolated and observe data/disconnect, not CONNECT', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateCountSubscription'
            ? { subscriptionId: 'isolated-count', count: 4 }
            : {});
        const consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});
        const resource = new CountSubscription(countQuery);
        const throwing = jest.fn(() => { throw new Error('count listener failed'); });
        const observing = jest.fn();

        const releaseThrowing = resource.subscribe(throwing);
        const releaseObserving = resource.subscribe(observing);
        expect(throwing).not.toHaveBeenCalled();
        expect(observing).not.toHaveBeenCalled();
        await flushMicrotasks();
        expect(throwing).toHaveBeenCalledTimes(1);
        expect(observing).toHaveBeenCalledTimes(1);

        for (const listener of [...controller.eventListeners.close]) listener(null);
        expect(resource.getCount()).toBeNull();
        expect(throwing).toHaveBeenCalledTimes(2);
        expect(observing).toHaveBeenCalledTimes(2);
        expect(consoleError).toHaveBeenCalledWith(
            'CountSubscription subscriber failed:',
            expect.objectContaining({ message: 'count listener failed' }),
        );

        expect(() => releaseThrowing()).not.toThrow();
        expect(() => releaseObserving()).not.toThrow();
        await flushMicrotasks();
        consoleError.mockRestore();
    });

    test('deletes a late create response after the last subscriber leaves', async () => {
        const controller = DataSyncController.getInstance();
        let resolveCreate;
        controller.sendMessage = jest.fn(payload => payload.tag === 'CreateCountSubscription'
            ? new Promise(resolve => { resolveCreate = resolve; })
            : Promise.resolve({}));
        const resource = new CountSubscription(countQuery);

        const release = resource.subscribe(() => {});
        release();
        await flushMicrotasks();
        resolveCreate({ subscriptionId: 'late-count', count: 42 });
        await flushMicrotasks();

        expect(resource.getCount()).toBeNull();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateCountSubscription',
            'DeleteDataSubscription',
        ]);
    });

    test('creation errors become observable snapshot state', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn().mockRejectedValue(new Error('count denied'));
        const resource = new CountSubscription(countQuery);

        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('error');
        expect(resource.getSnapshot().error?.message).toBe('count denied');
        release();
        await flushMicrotasks();
    });

    test('public subscribers remains a live Set while duplicate retains are counted separately', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateCountSubscription'
            ? { subscriptionId: 'public-count-listeners', count: 1 }
            : {});
        const resource = new CountSubscription(countQuery);
        const callback = jest.fn();
        const external = jest.fn();
        resource.subscribers.add(external);
        const releaseFirst = resource.subscribe(callback);
        const releaseSecond = resource.subscribe(callback);
        await flushMicrotasks();

        releaseFirst();
        expect(resource.subscribers.has(callback)).toBe(true);
        for (const listener of [...controller.eventListeners.message]) {
            listener({ tag: 'DidChangeCount', subscriptionId: 'public-count-listeners', count: 2 });
        }
        expect(callback).toHaveBeenCalled();
        expect(external).toHaveBeenCalled();

        releaseSecond();
        await flushMicrotasks();
        expect(resource.subscribers.has(callback)).toBe(false);
        expect(resource.getSnapshot().status).toBe('live');
        resource.subscribers.delete(external);
        await resource.close();
    });

    test('a render-held count resource is never evicted and duplicated before its commit', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateCountSubscription'
            ? { subscriptionId: `count-${payload.query.table}`, count: 0 }
            : {});
        const releases = [];
        for (let index = 0; index < 120; index++) {
            releases.push(CountSubscriptionStore.get({ ...countQuery, table: `active_count_${index}` }).subscribe(() => {}));
        }
        await flushMicrotasks();

        const laterCommitted = CountSubscriptionStore.get({ ...countQuery, table: 'later_count' });
        expect(CountSubscriptionStore.get({ ...countQuery, table: 'later_count' })).toBe(laterCommitted);
        expect(CountSubscriptionStore.queryMap.has(
            `count:${dataSubscriptionKey({ ...countQuery, table: 'later_count' })}`,
        )).toBe(false);
        for (let index = 0; index < 150; index++) {
            CountSubscriptionStore.get({ ...countQuery, table: `idle_count_${index}` });
        }

        expect(CountSubscriptionStore.get({ ...countQuery, table: 'later_count' })).toBe(laterCommitted);
        const releaseLater = laterCommitted.subscribe(() => {});
        expect(CountSubscriptionStore.queryMap.has(
            `count:${dataSubscriptionKey({ ...countQuery, table: 'later_count' })}`,
        )).toBe(true);
        const releaseShared = CountSubscriptionStore
            .get({ ...countQuery, table: 'later_count' })
            .subscribe(() => {});
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.filter(([payload]) =>
            payload.tag === 'CreateCountSubscription'
            && payload.query.table === 'later_count')).toHaveLength(1);

        releaseLater();
        releaseShared();
        releases.forEach(release => release());
        await flushMicrotasks();
        CountSubscriptionStore.queryMap.clear();
    });

    test('a render-held count resource cannot commit in a different JWT scope', () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'count-user-a';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        try {
            const oldResource = CountSubscriptionStore.get(countQuery);
            expect(DataSyncController.instance).toBeNull();

            jwt = 'count-user-b';
            const callback = jest.fn();
            const release = oldResource.subscribe(callback);

            expect(DataSyncController.instance).toBeNull();
            expect(oldResource.getCount()).toBeNull();
            expect(oldResource.getSnapshot().status).toBe('closed');
            expect(callback).not.toHaveBeenCalled();
            const newResource = CountSubscriptionStore.get(countQuery);
            expect(newResource).not.toBe(oldResource);
            release();
        } finally {
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });
});
