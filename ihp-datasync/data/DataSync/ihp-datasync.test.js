import {
    DataSubscription,
    DataSyncController,
    initIHPBackend,
    createRecord,
    updateRecord,
    deleteRecord,
} from './ihp-datasync.js';
import { DataSubscriptionStore, dataSubscriptionKey } from './data-subscription-store.js';
import { Transaction, withTransaction } from './transaction.js';
import { jest } from '@jest/globals';

function query(overrides = {}) {
    return {
        table: 'tasks',
        selectedColumns: { tag: 'SelectAll' },
        whereCondition: null,
        orderByClause: [],
        distinctOnColumn: null,
        limit: null,
        offset: null,
        ...overrides,
    };
}

async function flushMicrotasks(rounds = 8) {
    for (let index = 0; index < rounds; index++) {
        await Promise.resolve();
    }
}

describe('DataSubscription external store', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
        DataSyncController.ihpBackendHost = null;
        DataSubscriptionStore.queryMap.clear();
        DataSubscriptionStore.cache.clear();
    });

    test('constructing and looking up resources does not create a controller', () => {
        const direct = new DataSubscription(query());
        const stored = DataSubscriptionStore.get(query({ table: 'stored_tasks' }));

        expect(DataSyncController.instance).toBeNull();
        expect(direct.getSnapshot().status).toBe('idle');
        expect(stored.getSnapshot().status).toBe('idle');
    });

    test('registry lookup is render-pure and snapshots the mutable query', () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn();
        const mutableQuery = query();

        const resource = DataSubscriptionStore.get(mutableQuery);
        mutableQuery.limit = 1;

        expect(controller.sendMessage).not.toHaveBeenCalled();
        expect(resource.query.limit).toBeNull();
        expect(resource.cache).toBeNull();
        expect(resource.getSnapshot()).toBe(resource.getSnapshot());
        expect(resource.getSnapshot()).toEqual({ data: null, status: 'idle', error: null });
    });

    test('equal queries share one create and delete only after the final release', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscription', subscriptionId: 'shared', revision: 0, result: [] }
            : { tag: 'DidDeleteDataSubscription', subscriptionId: payload.subscriptionId });

        const first = DataSubscriptionStore.get(query());
        const second = DataSubscriptionStore.get(query());
        expect(second).toBe(first);

        const releaseFirst = first.subscribe(() => {});
        const releaseSecond = second.subscribe(() => {});
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.filter(([payload]) => payload.tag === 'CreateDataSubscription')).toHaveLength(1);

        releaseFirst();
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.filter(([payload]) => payload.tag === 'DeleteDataSubscription')).toHaveLength(0);

        releaseSecond();
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.filter(([payload]) => payload.tag === 'DeleteDataSubscription')).toHaveLength(1);
        expect(DataSubscriptionStore.queryMap.size).toBe(0);
    });

    test('a second imperative subscriber immediately receives the current shared snapshot', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'shared-current', revision: 0, result: [{ id: 'current' }] }
            : {});
        const resource = DataSubscriptionStore.get(query());
        const releaseFirst = resource.subscribe(() => {});
        await flushMicrotasks();

        const secondSubscriber = jest.fn();
        const releaseSecond = resource.subscribe(secondSubscriber);
        expect(secondSubscriber).toHaveBeenCalledTimes(1);
        expect(secondSubscriber).toHaveBeenCalledWith([{ id: 'current' }]);

        releaseFirst();
        releaseSecond();
        await flushMicrotasks();
    });

    test('a first legacy subscriber gets server data but no synchronous lifecycle value', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'legacy-callback', revision: 0, result: [{ id: 'ready' }] }
            : {});
        const resource = new DataSubscription(query());
        const callback = jest.fn();

        const release = resource.subscribe(callback);
        expect(callback).not.toHaveBeenCalled();
        await flushMicrotasks();
        expect(callback).toHaveBeenCalledTimes(1);
        expect(callback).toHaveBeenLastCalledWith([{ id: 'ready' }]);

        for (const listener of [...controller.eventListeners.close]) listener(null);
        expect(callback).toHaveBeenCalledTimes(1);
        release();
        await flushMicrotasks();
        expect(callback).toHaveBeenCalledTimes(1);
    });

    test('one throwing legacy subscriber cannot block another or prevent cleanup', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'throwing-listener', revision: 0, result: [{ id: 'safe' }] }
            : {});
        const consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});
        const resource = new DataSubscription(query());
        const throwing = jest.fn(() => { throw new Error('listener failed'); });
        const observing = jest.fn();

        const releaseThrowing = resource.subscribe(throwing);
        const releaseObserving = resource.subscribe(observing);
        await flushMicrotasks();

        expect(throwing).toHaveBeenCalledWith([{ id: 'safe' }]);
        expect(observing).toHaveBeenCalledWith([{ id: 'safe' }]);
        expect(consoleError).toHaveBeenCalledWith(
            'DataSubscription subscriber failed:',
            expect.objectContaining({ message: 'listener failed' }),
        );
        expect(() => releaseThrowing()).not.toThrow();
        expect(() => releaseObserving()).not.toThrow();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('closed');
        consoleError.mockRestore();
    });

    test('mutating the public query cannot alter create or refresh transport queries', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscription', subscriptionId: 'stable-query', result: [] }
            : { result: [] });
        const resource = new DataSubscription(query());
        resource.query.limit = 1;
        resource.query.orderByClause.push({ orderByColumn: 'createdAt', orderByDirection: 'Desc' });
        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        resource.onCreate({ id: 'invalidate' });
        await flushMicrotasks();

        const transportedQueries = controller.sendMessage.mock.calls
            .filter(([payload]) => payload.query !== undefined)
            .map(([payload]) => payload.query);
        expect(transportedQueries).toHaveLength(2);
        for (const transportedQuery of transportedQueries) {
            expect(transportedQuery.limit).toBeNull();
            expect(transportedQuery.orderByClause).toEqual([]);
            expect(Object.isFrozen(transportedQuery)).toBe(true);
        }
        release();
        await flushMicrotasks();
    });

    test('StrictMode release and retain in one turn reuses the server subscription', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscription', subscriptionId: 'strict', revision: 0, result: [] }
            : { tag: 'DidDeleteDataSubscription', subscriptionId: payload.subscriptionId });
        const resource = DataSubscriptionStore.get(query());

        const firstRelease = resource.subscribe(() => {});
        firstRelease();
        const finalRelease = resource.subscribe(() => {});
        await flushMicrotasks();

        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['CreateDataSubscription']);
        finalRelease();
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateDataSubscription',
            'DeleteDataSubscription',
        ]);
    });

    test('the same callback can be retained twice without under-counting', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { subscriptionId: 'duplicate-listener', revision: 0, result: [] }
            : {});
        const resource = new DataSubscription(query());
        const callback = jest.fn();

        const releaseFirst = resource.subscribe(callback);
        const releaseSecond = resource.subscribe(callback);
        await flushMicrotasks();
        releaseFirst();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('live');

        releaseSecond();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('closed');
    });

    test('a create response arriving after final release is deleted and cannot resurrect state', async () => {
        const controller = DataSyncController.getInstance();
        let resolveCreate;
        controller.sendMessage = jest.fn(payload => {
            if (payload.tag === 'CreateDataSubscription') {
                return new Promise(resolve => { resolveCreate = resolve; });
            }
            return Promise.resolve({ tag: 'DidDeleteDataSubscription' });
        });
        const resource = new DataSubscription(query());

        const release = resource.subscribe(() => {});
        release();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('closed');

        resolveCreate({ subscriptionId: 'late', revision: 0, result: [{ id: 'ghost' }] });
        await flushMicrotasks();

        expect(resource.getRecords()).toBeNull();
        expect(resource.subscriptionId).toBeNull();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateDataSubscription',
            'DeleteDataSubscription',
        ]);
    });

    test('a render-held resource reopened after full release is evicted again', async () => {
        const controller = DataSyncController.getInstance();
        let createNumber = 0;
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? {
                tag: 'DidCreateDataSubscriptionV2',
                subscriptionId: `reopened-${++createNumber}`,
                revision: 0,
                result: [],
            }
            : { tag: 'DidDeleteDataSubscription', subscriptionId: payload.subscriptionId });

        const resource = DataSubscriptionStore.get(query({ table: 'reopened_tasks' }));
        const releaseFirst = resource.subscribe(() => {});
        await flushMicrotasks();
        releaseFirst();
        await flushMicrotasks();
        expect(DataSubscriptionStore.queryMap.size).toBe(0);

        const releaseSecond = resource.subscribe(() => {});
        await flushMicrotasks();
        expect(DataSubscriptionStore.queryMap.size).toBe(1);
        releaseSecond();
        await flushMicrotasks();

        expect(DataSubscriptionStore.queryMap.size).toBe(0);
        expect(controller.sendMessage.mock.calls.filter(([payload]) =>
            payload.tag === 'CreateDataSubscription')).toHaveLength(2);
        expect(controller.sendMessage.mock.calls.filter(([payload]) =>
            payload.tag === 'DeleteDataSubscription')).toHaveLength(2);
    });

    test('authoritative replacements preserve server ordering and ignore old revisions', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async () => ({
            subscriptionId: 'revisions',
            revision: 0,
            result: [{ id: 'initial' }],
        }));
        const resource = new DataSubscription(query({ limit: 1 }));
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        const initialSnapshot = resource.getSnapshot();
        expect(resource.getSnapshot()).toBe(initialSnapshot);
        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'revisions',
            revision: 2,
            result: [{ id: 'new', rank: 1 }, { id: 'displaced', rank: 2 }],
        });
        const latestSnapshot = resource.getSnapshot();
        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'revisions',
            revision: 1,
            result: [{ id: 'stale' }],
        });

        expect(resource.getSnapshot()).toBe(latestSnapshot);
        expect(resource.getRecords()).toEqual([
            { id: 'new', rank: 1 },
            { id: 'displaced', rank: 2 },
        ]);
        expect(Object.isFrozen(resource.getRecords())).toBe(false);
        resource.getRecords().push({ id: 'locally-mutable-for-api-compatibility' });
        expect(resource.getRecords()).toHaveLength(3);
        release();
        await flushMicrotasks();
    });

    test('the V2 create acknowledgement enables snapshots without waiting for a replacement', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'v2', revision: 0, result: [] }
            : {});
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        resource.receiveUpdate({ tag: 'DidInsert', subscriptionId: 'v2', record: { id: 'ignored-delta' } });
        await flushMicrotasks();

        expect(controller.sendMessage.mock.calls[0][0]).toMatchObject({
            tag: 'CreateDataSubscription',
            protocolVersion: 1,
        });
        expect(controller.sendMessage.mock.calls.filter(([payload]) => payload.tag === 'DataSyncQuery')).toHaveLength(0);
        release();
        await flushMicrotasks();
    });

    test('the first replacement is authoritative even if its revision equals the legacy baseline', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async () => ({
            tag: 'DidCreateDataSubscription',
            subscriptionId: 'upgrade-on-replacement',
            result: [{ id: 'baseline' }],
        }));
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'upgrade-on-replacement',
            revision: 0,
            result: [{ id: 'replacement' }],
        });
        expect(resource.getRecords()).toEqual([{ id: 'replacement' }]);
        release();
        await flushMicrotasks();
    });

    test('public subscribers remains a live writable compatibility array', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'public-listeners', revision: 0, result: [] }
            : {});
        const resource = new DataSubscription(query());
        const externallyAdded = jest.fn();
        resource.subscribers.push(externallyAdded);
        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        externallyAdded.mockClear();

        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'public-listeners',
            revision: 1,
            result: [{ id: 'next' }],
        });
        expect(externallyAdded).toHaveBeenCalledWith([{ id: 'next' }]);
        release();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('live');
        resource.subscribers.length = 0;
        await resource.close();
    });

    test('imperative ownership transfers to the first ref-counted subscriber', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'imperative-transfer', revision: 0, result: [] }
            : {});
        const resource = new DataSubscription(query());

        await resource.createOnServer();
        await flushMicrotasks();
        expect(resource.getSnapshot().status).toBe('live');
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['CreateDataSubscription']);

        const release = resource.subscribe(() => {});
        release();
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateDataSubscription',
            'DeleteDataSubscription',
        ]);
    });

    test('a never-subscribed imperative resource remains owned until explicit close', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'imperative-close', revision: 0, result: [] }
            : {});
        const resource = new DataSubscription(query());

        await resource.createOnServer();
        await flushMicrotasks();
        expect(controller.sendMessage).toHaveBeenCalledTimes(1);
        await resource.close();
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'CreateDataSubscription',
            'DeleteDataSubscription',
        ]);
    });

    test('closing before the first response settles createOnServerPromise', async () => {
        const resource = new DataSubscription(query());
        const initialCreate = resource.createOnServerPromise;

        await resource.close();
        await expect(initialCreate).rejects.toThrow('closed before its initial server snapshot arrived');
    });

    test('a render-held resource is never evicted and duplicated before its commit', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
            ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: `active-${payload.query.table}`, revision: 0, result: [] }
            : {});
        const releases = [];
        for (let index = 0; index < 120; index++) {
            releases.push(DataSubscriptionStore.get(query({ table: `active_${index}` })).subscribe(() => {}));
        }
        await flushMicrotasks();

        const laterCommitted = DataSubscriptionStore.get(query({ table: 'later_committed' }));
        expect(DataSubscriptionStore.get(query({ table: 'later_committed' }))).toBe(laterCommitted);
        expect(DataSubscriptionStore.queryMap.has(
            dataSubscriptionKey(query({ table: 'later_committed' })),
        )).toBe(false);
        for (let index = 0; index < 150; index++) {
            DataSubscriptionStore.get(query({ table: `render_only_${index}` }));
        }
        expect(DataSubscriptionStore.get(query({ table: 'later_committed' }))).toBe(laterCommitted);

        const releaseLater = laterCommitted.subscribe(() => {});
        expect(DataSubscriptionStore.queryMap.has(
            dataSubscriptionKey(query({ table: 'later_committed' })),
        )).toBe(true);
        const releaseShared = DataSubscriptionStore
            .get(query({ table: 'later_committed' }))
            .subscribe(() => {});
        await flushMicrotasks();
        expect(controller.sendMessage.mock.calls.filter(([payload]) =>
            payload.tag === 'CreateDataSubscription'
            && payload.query.table === 'later_committed')).toHaveLength(1);

        releaseLater();
        releaseShared();
        releases.forEach(release => release());
        await flushMicrotasks();
        DataSubscriptionStore.queryMap.clear();
    });

    test('JWT scope rotation is lazy until commit and closes the old resource and socket', async () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'old-token';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        const prototypeSend = jest.spyOn(DataSyncController.prototype, 'sendMessage');
        try {
            const oldController = DataSyncController.getInstance();
            oldController.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
                ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'old-scope', revision: 0, result: [{ id: 'old-secret' }] }
                : {});
            const oldSocket = { close: jest.fn(), onclose: null, onmessage: null };
            oldController.connection = oldSocket;
            const oldResource = DataSubscriptionStore.get(query());
            const releaseOld = oldResource.subscribe(() => {});
            await flushMicrotasks();
            expect(oldResource.getRecords()).toEqual([{ id: 'old-secret' }]);

            jwt = 'new-token';
            const newResource = DataSubscriptionStore.get(query());
            expect(newResource).not.toBe(oldResource);
            expect(DataSyncController.instance).toBe(oldController);
            expect(oldSocket.close).not.toHaveBeenCalled();

            prototypeSend.mockImplementation(async payload => payload.tag === 'CreateDataSubscription'
                ? { tag: 'DidCreateDataSubscriptionV2', subscriptionId: 'new-scope', revision: 0, result: [{ id: 'new-row' }] }
                : {});
            const releaseNew = newResource.subscribe(() => {});
            await flushMicrotasks();

            expect(DataSyncController.instance).not.toBe(oldController);
            expect(oldController.retired).toBe(true);
            expect(oldSocket.close).toHaveBeenCalledTimes(1);
            expect(oldResource.getRecords()).toBeNull();
            expect(oldResource.getSnapshot().status).toBe('closed');
            expect(newResource.getRecords()).toEqual([{ id: 'new-row' }]);

            oldController.onMessage({ data: JSON.stringify({
                tag: 'DidReplaceDataSubscription',
                subscriptionId: 'old-scope',
                revision: 99,
                result: [{ id: 'late-old-secret' }],
            }) });
            expect(oldResource.getRecords()).toBeNull();

            releaseOld();
            releaseNew();
            await flushMicrotasks();
        } finally {
            prototypeSend.mockRestore();
            if (originalLocalStorage === undefined) {
                delete globalThis.localStorage;
            } else {
                globalThis.localStorage = originalLocalStorage;
            }
        }
    });

    test('a cached render-held resource cannot commit or expose cache in a new JWT scope', async () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'render-user-a';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        try {
            const resourceQuery = query({ table: 'private_tasks' });
            const oldKey = dataSubscriptionKey(resourceQuery);
            DataSubscriptionStore.cache.set(oldKey, [{ id: 'user-a-secret' }]);
            const oldResource = DataSubscriptionStore.get(resourceQuery);
            const staleImperative = DataSubscriptionStore.get(query({ table: 'imperative_private_tasks' }));
            expect(oldResource.getRecords()).toEqual([{ id: 'user-a-secret' }]);
            expect(DataSyncController.instance).toBeNull();

            jwt = 'render-user-b';
            const callback = jest.fn();
            const release = oldResource.subscribe(callback);
            const releaseAgain = oldResource.subscribe(() => {});

            expect(DataSyncController.instance).toBeNull();
            expect(oldResource.getRecords()).toBeNull();
            expect(oldResource.getSnapshot().status).toBe('closed');
            expect(callback).not.toHaveBeenCalled();
            expect(DataSubscriptionStore.queryMap.has(oldKey)).toBe(false);
            await expect(staleImperative.createOnServer()).rejects.toThrow('scope changed');
            expect(DataSyncController.instance).toBeNull();

            const newResource = DataSubscriptionStore.get(resourceQuery);
            expect(newResource).not.toBe(oldResource);
            expect(newResource.getRecords()).toBeNull();
            release();
            releaseAgain();
        } finally {
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });

    test('the JWT-scoped record cache remains bounded independently of the weak render registry', async () => {
        const originalLocalStorage = globalThis.localStorage;
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? 'cache-user' : null };
        try {
            const controller = DataSyncController.getInstance();
            controller.sendMessage = jest.fn(async payload => payload.tag === 'CreateDataSubscription'
                ? {
                    tag: 'DidCreateDataSubscriptionV2',
                    subscriptionId: `cache-${payload.query.table}`,
                    revision: 0,
                    result: [{ id: payload.query.table }],
                }
                : {});
            const releases = [];
            for (let index = 0; index < 105; index++) {
                const resource = DataSubscriptionStore.get(query({ table: `cached_${index}` }));
                releases.push(resource.subscribe(() => {}));
            }
            await flushMicrotasks();

            expect(DataSubscriptionStore.cache.size).toBe(100);
            for (let index = 0; index < 5; index++) {
                expect(DataSubscriptionStore.cache.has(
                    dataSubscriptionKey(query({ table: `cached_${index}` })),
                )).toBe(false);
            }
            releases.forEach(release => release());
            await flushMicrotasks();
        } finally {
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });

    test('reconfiguring the same backend keeps the transport while a changed backend retires it', () => {
        DataSyncController.ihpBackendHost = 'https://api.example.test';
        const controller = DataSyncController.getInstance();
        const socket = { close: jest.fn(), onclose: null, onmessage: null };
        controller.connection = socket;

        initIHPBackend({ host: 'https://api.example.test' });
        expect(DataSyncController.instance).toBe(controller);
        expect(socket.close).not.toHaveBeenCalled();

        initIHPBackend({ host: 'https://other.example.test' });
        expect(socket.close).toHaveBeenCalledTimes(1);
        expect(controller.retired).toBe(true);
        expect(DataSyncController.instance).toBeNull();
    });

    test('connection errors are observable snapshot transitions', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn().mockRejectedValue(new Error('denied'));
        const resource = new DataSubscription(query());
        const before = resource.getSnapshot();
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        expect(resource.getSnapshot()).not.toBe(before);
        expect(resource.getSnapshot().status).toBe('error');
        expect(resource.getSnapshot().error).toEqual(expect.objectContaining({ message: expect.stringContaining('denied') }));
        release();
        await flushMicrotasks();
    });

    test('disconnect keeps the last snapshot and reconnect creates exactly once', async () => {
        const controller = DataSyncController.getInstance();
        let createNumber = 0;
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'CreateDataSubscription') {
                createNumber++;
                return {
                    subscriptionId: `subscription-${createNumber}`,
                    revision: 0,
                    result: [{ id: `snapshot-${createNumber}` }],
                };
            }
            return {};
        });
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();
        expect(resource.isClosed).toBe(false);
        expect(resource.isConnected).toBe(true);

        for (const listener of [...controller.eventListeners.close]) listener(null);
        expect(resource.getSnapshot()).toEqual({
            data: [{ id: 'snapshot-1' }],
            status: 'reconnecting',
            error: null,
        });
        expect(resource.isClosed).toBe(true);
        expect(resource.isConnected).toBe(false);
        for (const listener of [...controller.eventListeners.reconnect]) listener();
        for (const listener of [...controller.eventListeners.reconnect]) listener();
        await flushMicrotasks();

        expect(createNumber).toBe(2);
        expect(resource.getRecords()).toEqual([{ id: 'snapshot-2' }]);
        expect(resource.isClosed).toBe(false);
        expect(resource.isConnected).toBe(true);
        resource.onMessage({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'subscription-1',
            revision: 99,
            result: [{ id: 'obsolete' }],
        });
        expect(resource.getRecords()).toEqual([{ id: 'snapshot-2' }]);
        release();
        await flushMicrotasks();
    });

    test('legacy delta messages coalesce exact server refreshes instead of mutating records', async () => {
        const controller = DataSyncController.getInstance();
        let resolveFirstRefresh;
        let refreshCount = 0;
        controller.sendMessage = jest.fn(payload => {
            if (payload.tag === 'CreateDataSubscription') {
                return Promise.resolve({ subscriptionId: 'legacy', result: [{ id: 'server-a' }] });
            }
            if (payload.tag === 'DataSyncQuery') {
                refreshCount++;
                if (refreshCount === 1) {
                    return new Promise(resolve => { resolveFirstRefresh = resolve; });
                }
                return Promise.resolve({ result: [{ id: 'server-c' }] });
            }
            return Promise.resolve({});
        });
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        resource.receiveUpdate({ tag: 'DidInsert', subscriptionId: 'legacy', record: { id: 'client-b' } });
        resource.receiveUpdate({ tag: 'DidDelete', subscriptionId: 'legacy', id: 'server-a' });
        expect(resource.getRecords()).toEqual([{ id: 'server-a' }]);
        expect(refreshCount).toBe(1);

        resolveFirstRefresh({ result: [{ id: 'server-b' }] });
        await flushMicrotasks();
        expect(refreshCount).toBe(2);
        expect(resource.getRecords()).toEqual([{ id: 'server-c' }]);
        release();
        await flushMicrotasks();
    });

    test('an in-flight legacy refresh cannot overwrite a newer revisioned replacement', async () => {
        const controller = DataSyncController.getInstance();
        let resolveRefresh;
        controller.sendMessage = jest.fn(payload => {
            if (payload.tag === 'CreateDataSubscription') {
                return Promise.resolve({
                    tag: 'DidCreateDataSubscription',
                    subscriptionId: 'legacy-upgrade-race',
                    result: [{ id: 'baseline' }],
                });
            }
            if (payload.tag === 'DataSyncQuery') {
                return new Promise(resolve => { resolveRefresh = resolve; });
            }
            return Promise.resolve({});
        });
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        resource.receiveUpdate({
            tag: 'DidInsert',
            subscriptionId: 'legacy-upgrade-race',
            record: { id: 'invalidate' },
        });
        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'legacy-upgrade-race',
            revision: 1,
            result: [{ id: 'authoritative' }],
        });
        resolveRefresh({ result: [{ id: 'stale-refetch' }] });
        await flushMicrotasks();

        expect(resource.getRecords()).toEqual([{ id: 'authoritative' }]);
        release();
        await flushMicrotasks();
    });
});

describe('server-authoritative CRUD', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
    });

    test('create, update and delete never mutate subscription snapshots locally', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => {
            switch (payload.tag) {
                case 'CreateDataSubscription':
                    return { subscriptionId: 'crud', revision: 0, result: [{ id: 'old', title: 'Old' }] };
                case 'CreateRecordMessage':
                    return { record: payload.record };
                case 'UpdateRecordMessage':
                    return { record: { id: payload.id, ...payload.patch } };
                default:
                    return {};
            }
        });
        const resource = new DataSubscription(query());
        const release = resource.subscribe(() => {});
        await flushMicrotasks();

        await createRecord('tasks', { id: 'new', title: 'New' });
        await updateRecord('tasks', 'old', { title: 'Changed' });
        await deleteRecord('tasks', 'old');
        expect(resource.getRecords()).toEqual([{ id: 'old', title: 'Old' }]);

        resource.receiveUpdate({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'crud',
            revision: 1,
            result: [{ id: 'new', title: 'New' }],
        });
        expect(resource.getRecords()).toEqual([{ id: 'new', title: 'New' }]);
        release();
        await flushMicrotasks();
    });

    test('a child create waits for the parent create it references', async () => {
        const controller = DataSyncController.getInstance();
        let resolveParent;
        const sentIds = [];
        controller.sendMessage = jest.fn(payload => {
            sentIds.push(payload.record.id);
            if (payload.record.id === 'parent') {
                return new Promise(resolve => { resolveParent = resolve; });
            }
            return Promise.resolve({ record: payload.record });
        });

        const parent = createRecord('parents', { id: 'parent', name: 'Parent' });
        await flushMicrotasks(2);
        const child = createRecord('children', { id: 'child', parentId: 'parent' });
        await flushMicrotasks(2);
        expect(sentIds).toEqual(['parent']);

        resolveParent({ record: { id: 'parent', name: 'Parent' } });
        await parent;
        await child;
        expect(sentIds).toEqual(['parent', 'child']);
    });

    test('a dependent delete is not sent after its pending create fails', async () => {
        const controller = DataSyncController.getInstance();
        let rejectCreate;
        controller.sendMessage = jest.fn(payload => payload.tag === 'CreateRecordMessage'
            ? new Promise((_resolve, reject) => { rejectCreate = reject; })
            : Promise.resolve({}));

        const createOutcome = createRecord('tasks', { id: 'ghost' }).catch(error => error);
        await flushMicrotasks(2);
        const deleteOutcome = deleteRecord('tasks', 'ghost').catch(error => error);
        await flushMicrotasks(2);
        rejectCreate(new Error('create failed'));
        await Promise.all([createOutcome, deleteOutcome]);

        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual(['CreateRecordMessage']);
    });
});

describe('transaction transport scope', () => {
    beforeEach(() => {
        DataSyncController.instance = null;
        DataSyncController.ihpBackendHost = null;
    });

    test('CRUD and query operations never move a transaction id to a replacement auth scope', async () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'transaction-user-a';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        try {
            const controller = DataSyncController.getInstance();
            const sentMessages = [];
            const socket = {
                onclose: null,
                onmessage: null,
                close: jest.fn(),
                send: jest.fn(serialized => {
                    const payload = JSON.parse(serialized);
                    sentMessages.push(payload);
                    if (payload.tag === 'StartTransaction') {
                        queueMicrotask(() => controller.onMessage({ data: JSON.stringify({
                            tag: 'DidStartTransaction',
                            requestId: payload.requestId,
                            transactionId: 'transaction-a',
                        }) }));
                    }
                }),
            };
            controller.connection = socket;
            const transaction = new Transaction();
            await transaction.start();
            const transactionQuery = transaction.query('tasks');
            expect(() => transactionQuery.subscribe(() => {})).toThrow(
                'subscriptions are not supported inside a transaction',
            );

            jwt = 'transaction-user-b';
            const update = transaction.updateRecord('tasks', 'task-id', { title: 'forbidden' });
            const fetch = transactionQuery.fetch();

            await expect(update).rejects.toThrow('transport scope');
            await expect(fetch).rejects.toThrow('transport scope');
            expect(sentMessages.map(message => message.tag)).toEqual(['StartTransaction']);
            expect(sentMessages.some(message => message.transactionId === 'transaction-a')).toBe(false);
            expect(controller.retired).toBe(true);
            expect(socket.close).toHaveBeenCalledTimes(1);
            expect(DataSyncController.instance).toBeNull();
        } finally {
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });

    test('withTransaction preserves the callback error after transport close clears the id', async () => {
        const controller = DataSyncController.getInstance();
        controller.sendMessage = jest.fn(async payload => {
            if (payload.tag === 'StartTransaction') {
                return { transactionId: 'transaction-to-close' };
            }
            throw new Error(`Unexpected ${payload.tag}`);
        });
        const callbackError = new Error('callback failed during auth handoff');

        const outcome = withTransaction(async () => {
            for (const listener of [...controller.eventListeners.close]) {
                listener(null);
            }
            throw callbackError;
        });

        await expect(outcome).rejects.toBe(callbackError);
        expect(controller.sendMessage.mock.calls.map(([payload]) => payload.tag)).toEqual([
            'StartTransaction',
        ]);
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

    test('an unrelated push message cannot clear another request timeout', async () => {
        const controller = DataSyncController.getInstance();
        controller.connection = { send: jest.fn(), close: jest.fn() };
        const first = controller.sendMessage({ tag: 'FirstRequest' });
        const second = controller.sendMessage({ tag: 'SecondRequest' });

        controller.onMessage({ data: JSON.stringify({
            tag: 'DidReplaceDataSubscription',
            subscriptionId: 'other',
            revision: 1,
            result: [],
        }) });
        expect(controller.pendingRequests).toHaveLength(2);

        controller.onMessage({ data: JSON.stringify({ tag: 'DataSyncResult', requestId: 0, result: [] }) });
        await expect(first).resolves.toMatchObject({ requestId: 0 });
        expect(controller.pendingRequests[0].timeout).not.toBeNull();

        controller.onClose(null);
        await expect(second).rejects.toThrow('closed before the server responded');
    });

    test('sendMessage rejects and retires a controller captured by an old auth scope', async () => {
        const originalLocalStorage = globalThis.localStorage;
        let jwt = 'controller-user-a';
        globalThis.localStorage = { getItem: key => key === 'ihp_jwt' ? jwt : null };
        try {
            const controller = DataSyncController.getInstance();
            const socket = {
                send: jest.fn(),
                close: jest.fn(),
                onclose: null,
                onmessage: null,
            };
            controller.connection = socket;
            jwt = 'controller-user-b';

            await expect(controller.sendMessage({ tag: 'MustNotSend' })).rejects.toThrow('transport scope');
            expect(socket.send).not.toHaveBeenCalled();
            expect(socket.close).toHaveBeenCalledTimes(1);
            expect(controller.retired).toBe(true);
            expect(DataSyncController.instance).toBeNull();
        } finally {
            if (originalLocalStorage === undefined) delete globalThis.localStorage;
            else globalThis.localStorage = originalLocalStorage;
        }
    });

    test('retirement blocks reentrant controller acquisition and deterministically leaves null', () => {
        const controller = DataSyncController.getInstance();
        const socket = { send: jest.fn(), close: jest.fn(), onclose: null, onmessage: null };
        controller.connection = socket;
        let reentrantError = null;
        controller.addEventListener('close', () => {
            try {
                DataSyncController.getInstance();
            } catch (error) {
                reentrantError = error;
            }
        });
        const instanceEvents = [];
        const removeInstanceListener = DataSyncController.addInstanceListener(instance => {
            instanceEvents.push(instance);
        });

        DataSyncController.retireCurrentTransport();

        expect(reentrantError).toEqual(expect.objectContaining({
            message: expect.stringContaining('while the current transport is being retired'),
        }));
        expect(instanceEvents).toEqual([null]);
        expect(DataSyncController.instance).toBeNull();
        expect(controller.retired).toBe(true);
        expect(socket.close).toHaveBeenCalledTimes(1);
        removeInstanceListener();
    });

    test('one throwing controller listener cannot stop later listeners', () => {
        const controller = DataSyncController.getInstance();
        const consoleError = jest.spyOn(console, 'error').mockImplementation(() => {});
        const observer = jest.fn();
        controller.addEventListener('message', () => { throw new Error('message listener failed'); });
        controller.addEventListener('message', observer);

        expect(() => controller.onMessage({ data: JSON.stringify({ tag: 'Push' }) })).not.toThrow();
        expect(observer).toHaveBeenCalledWith({ tag: 'Push' });
        expect(consoleError).toHaveBeenCalledWith(
            'DataSync message listener failed:',
            expect.objectContaining({ message: 'message listener failed' }),
        );
        consoleError.mockRestore();
    });

    test('a timed-out request is rejected and removed', async () => {
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
            close() {}
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
            if (originalWebSocket === undefined) delete globalThis.WebSocket;
            else globalThis.WebSocket = originalWebSocket;
            if (originalLocation === undefined) delete globalThis.location;
            else globalThis.location = originalLocation;
            if (originalDocument === undefined) delete globalThis.document;
            else globalThis.document = originalDocument;
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
            if (originalWebSocket === undefined) delete globalThis.WebSocket;
            else globalThis.WebSocket = originalWebSocket;
            if (originalLocation === undefined) delete globalThis.location;
            else globalThis.location = originalLocation;
            if (originalDocument === undefined) delete globalThis.document;
            else globalThis.document = originalDocument;
        }
    });
});
