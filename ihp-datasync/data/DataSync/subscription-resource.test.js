import { jest } from '@jest/globals';

import { rowSubscriptionPolicy } from './subscription-machine.js';
import { createSubscriptionResource } from './subscription-resource.js';

const flushPromises = async () => {
    for (let index = 0; index < 6; index += 1) {
        await Promise.resolve();
    }
};

function deferred() {
    let resolve;
    let reject;
    const promise = new Promise((resolvePromise, rejectPromise) => {
        resolve = resolvePromise;
        reject = rejectPromise;
    });
    return { promise, resolve, reject };
}

function fakeTransport(scopeKey = 'scope-a') {
    let current = true;
    const listeners = new Set();
    const unsubscribes = [];
    const transport = {
        scopeKey,
        request: jest.fn(),
        subscribe: jest.fn(listener => {
            listeners.add(listener);
            const unsubscribe = jest.fn(() => listeners.delete(listener));
            unsubscribes.push(unsubscribe);
            return unsubscribe;
        }),
        isCurrent: jest.fn(() => current),
    };

    return {
        transport,
        listeners,
        unsubscribes,
        emit(event) {
            for (const listener of [...listeners]) {
                listener(event);
            }
        },
        setCurrent(value) {
            current = value;
        },
    };
}

function created(subscriptionId, value, revision = 0, mode = 'snapshot') {
    return { subscriptionId, value, revision, mode };
}

function makeHarness({
    scopeKey = 'scope-a',
    transports = [fakeTransport(scopeKey)],
    create = async () => created('subscription-1', [{ id: 'initial' }]),
    deleteSubscription = async () => {},
    refresh,
    decodeMessage = () => null,
    isRelevantMessage,
    publish,
    onState,
    initialData,
} = {}) {
    let currentScopeKeyValue = scopeKey;
    let nextTransport = 0;
    const idleChecks = [];
    const cacheWrites = [];
    const publications = [];
    const states = [];
    const errors = [];
    const evictions = [];
    const acquireTransport = jest.fn(() => {
        const fixture = transports[nextTransport];
        nextTransport += 1;
        if (fixture === undefined) {
            throw new Error('No fake transport available');
        }
        return fixture.transport;
    });
    const currentScopeKey = jest.fn(() => currentScopeKeyValue);
    const protocol = {
        create: jest.fn(create),
        delete: jest.fn(deleteSubscription),
        refresh: refresh === undefined ? undefined : jest.fn(refresh),
        decodeMessage: jest.fn(decodeMessage),
        isRelevantMessage: isRelevantMessage === undefined
            ? undefined
            : jest.fn(isRelevantMessage),
    };
    let resource;
    resource = createSubscriptionResource(protocol, {
        expectedScopeKey: scopeKey,
        currentScopeKey,
        acquireTransport,
        policy: rowSubscriptionPolicy,
        initialData,
        writeCache: value => cacheWrites.push(value),
        evict: () => evictions.push(resource),
        publish: (publication, state, previousState) => {
            publications.push({ publication, state, previousState });
            publish?.(publication, state, previousState, resource);
        },
        onState: state => {
            states.push(state);
            onState?.(state, resource);
        },
        reportError: (message, error) => errors.push({ message, error }),
        queueMicrotask: callback => idleChecks.push(callback),
    });

    return {
        resource,
        protocol,
        acquireTransport,
        currentScopeKey,
        idleChecks,
        cacheWrites,
        publications,
        states,
        errors,
        evictions,
        setCurrentScopeKey(value) {
            currentScopeKeyValue = value;
        },
    };
}

describe('subscription resource runtime', () => {
    test('scheduleCloseIfUnused defers closing a never-demanded resource', async () => {
        const harness = makeHarness();
        const initialFailure = harness.resource.initialResult.catch(error => error);

        harness.resource.scheduleCloseIfUnused();

        expect(harness.resource.getState().phase).toEqual({ tag: 'idle' });
        expect(harness.idleChecks).toHaveLength(1);
        expect(harness.acquireTransport).not.toHaveBeenCalled();
        expect(harness.evictions).toHaveLength(0);

        harness.idleChecks.shift()();

        expect(harness.resource.getState().phase).toEqual({ tag: 'closed' });
        expect(harness.acquireTransport).not.toHaveBeenCalled();
        expect(harness.evictions).toHaveLength(1);
        await expect(initialFailure).resolves.toMatchObject({
            message: rowSubscriptionPolicy.initialUnusedMessage,
        });
    });

    test('an irrelevant message skips scope validation and decoding', () => {
        const harness = makeHarness({
            isRelevantMessage: () => false,
            decodeMessage: () => {
                throw new Error('irrelevant message must not be decoded');
            },
        });
        harness.setCurrentScopeKey('scope-b');
        const message = { tag: 'DidChangeCount', subscriptionId: 'another-resource' };

        harness.resource.receiveMessage(message);

        expect(harness.protocol.isRelevantMessage).toHaveBeenCalledWith(
            message,
            harness.resource.getState(),
        );
        expect(harness.currentScopeKey).not.toHaveBeenCalled();
        expect(harness.protocol.decodeMessage).not.toHaveBeenCalled();
        expect(harness.resource.getState()).toMatchObject({
            scope: 'current',
            phase: { tag: 'idle' },
        });
    });

    test('an abandoned resource with no demand never acquires a transport', async () => {
        const harness = makeHarness();

        await flushPromises();
        expect(harness.acquireTransport).not.toHaveBeenCalled();
        expect(harness.protocol.create).not.toHaveBeenCalled();

        await harness.resource.close();
        await flushPromises();

        expect(harness.acquireTransport).not.toHaveBeenCalled();
        expect(harness.protocol.create).not.toHaveBeenCalled();
        expect(harness.resource.getState().phase).toEqual({ tag: 'closed' });
    });

    test('multiple consumers share one transport, listener, and create request', async () => {
        const fixture = fakeTransport();
        const createResult = deferred();
        const harness = makeHarness({
            transports: [fixture],
            create: () => createResult.promise,
        });

        harness.resource.updateDemand(1, false);
        harness.resource.updateDemand(2, false);
        harness.resource.updateDemand(2, true);
        await flushPromises();

        expect(harness.acquireTransport).toHaveBeenCalledTimes(1);
        expect(fixture.transport.subscribe).toHaveBeenCalledTimes(1);
        expect(harness.protocol.create).toHaveBeenCalledTimes(1);

        const value = [{ id: 'shared' }];
        createResult.resolve(created('shared-subscription', value, 4));
        await expect(harness.resource.initialResult).resolves.toBe(value);
        expect(harness.resource.getState().phase).toEqual({
            tag: 'live',
            subscriptionId: 'shared-subscription',
            mode: 'snapshot',
            revision: 4,
        });

        await harness.resource.close();
    });

    test('new demand strictly cancels the queued last-demand idle check', async () => {
        const createResult = deferred();
        const harness = makeHarness({ create: () => createResult.promise });

        harness.resource.updateDemand(1, false);
        await flushPromises();
        harness.resource.updateDemand(0, false);
        expect(harness.idleChecks).toHaveLength(1);

        harness.resource.updateDemand(1, false);
        harness.idleChecks.shift()();

        expect(harness.resource.getState().phase.tag).toBe('creating');
        expect(harness.evictions).toHaveLength(0);
        expect(harness.protocol.delete).not.toHaveBeenCalled();
        expect(harness.protocol.create).toHaveBeenCalledTimes(1);

        createResult.resolve(created('kept-alive', [{ id: 'kept' }]));
        await flushPromises();
        await harness.resource.close();
    });

    test('a late create is deleted on its original transport', async () => {
        const transportA = fakeTransport();
        const transportB = fakeTransport();
        const createA = deferred();
        const createB = deferred();
        const harness = makeHarness({
            transports: [transportA, transportB],
            create: transport => transport === transportA.transport
                ? createA.promise
                : createB.promise,
        });

        harness.resource.updateDemand(1, false);
        await flushPromises();
        harness.resource.updateDemand(0, false);
        harness.idleChecks.shift()();
        harness.resource.updateDemand(1, false);
        await flushPromises();

        expect(harness.acquireTransport).toHaveBeenCalledTimes(2);
        createA.resolve(created('late-a', [{ id: 'stale' }]));
        await flushPromises();

        expect(harness.protocol.delete)
            .toHaveBeenCalledWith(transportA.transport, 'late-a');
        expect(harness.protocol.delete)
            .not.toHaveBeenCalledWith(transportB.transport, 'late-a');
        expect(harness.resource.getSnapshot().data).toBeNull();

        createB.resolve(created('live-b', [{ id: 'current' }]));
        await flushPromises();
        await harness.resource.close();
    });

    test('an obsolete create task settles only after its stale remote id is deleted', async () => {
        const createResult = deferred();
        const deleteResult = deferred();
        const harness = makeHarness({
            create: () => createResult.promise,
            deleteSubscription: () => deleteResult.promise,
        });
        const initialFailure = harness.resource.initialResult.catch(error => error);

        harness.resource.updateDemand(1, false);
        const creating = harness.resource.ensureCreated();
        let createSettled = false;
        void creating.then(() => {
            createSettled = true;
        });
        harness.resource.updateDemand(0, false);
        harness.idleChecks.shift()();

        createResult.resolve(created('late-delete-barrier', [{ id: 'stale' }]));
        await flushPromises();
        expect(harness.protocol.delete).toHaveBeenCalledWith(
            expect.any(Object),
            'late-delete-barrier',
        );
        expect(createSettled).toBe(false);

        deleteResult.resolve();
        await creating;
        expect(createSettled).toBe(true);
        await expect(initialFailure).resolves.toEqual(expect.objectContaining({
            message: rowSubscriptionPolicy.initialUnusedMessage,
        }));
    });

    test('an obsolete create failure preserves the earlier close result', async () => {
        const createResult = deferred();
        const harness = makeHarness({ create: () => createResult.promise });
        const initialFailure = harness.resource.initialResult.catch(error => error);

        harness.resource.updateDemand(1, false);
        const creating = harness.resource.ensureCreated();
        harness.resource.updateDemand(0, false);
        harness.idleChecks.shift()();
        createResult.reject(new Error('late wire failure'));

        await expect(creating).resolves.toBeUndefined();
        await expect(initialFailure).resolves.toEqual(expect.objectContaining({
            message: rowSubscriptionPolicy.initialUnusedMessage,
        }));
        expect(harness.resource.getSnapshot().status).toBe('closed');
        expect(harness.protocol.delete).not.toHaveBeenCalled();
    });

    test('a scope race cannot publish or cache the late create value', async () => {
        const fixture = fakeTransport();
        const createResult = deferred();
        const harness = makeHarness({
            transports: [fixture],
            create: () => createResult.promise,
        });
        const initialFailure = harness.resource.initialResult.catch(error => error);

        harness.resource.updateDemand(1, false);
        await flushPromises();
        harness.setCurrentScopeKey('scope-b');
        const secret = [{ id: 'must-not-escape' }];
        createResult.resolve(created('stale-scope', secret, 1));
        await flushPromises();

        expect(harness.resource.getState().scope).toBe('stale');
        expect(harness.resource.getSnapshot()).toMatchObject({
            data: null,
            status: 'closed',
        });
        expect(harness.cacheWrites).not.toContain(secret);
        expect(harness.publications.some(({ state }) => state.snapshot.data === secret))
            .toBe(false);
        expect(harness.protocol.delete)
            .toHaveBeenCalledWith(fixture.transport, 'stale-scope');
        await expect(initialFailure).resolves.toBeInstanceOf(Error);
    });

    test('reconnect starts exactly one replacement create through the one listener', async () => {
        const fixture = fakeTransport();
        const firstCreate = deferred();
        const secondCreate = deferred();
        const creates = [firstCreate, secondCreate];
        const harness = makeHarness({
            transports: [fixture],
            create: () => creates.shift().promise,
        });

        harness.resource.updateDemand(1, false);
        await flushPromises();
        firstCreate.resolve(created('before-disconnect', [{ id: 'first' }]));
        await flushPromises();

        fixture.emit({ type: 'closed', scopeChanged: false });
        expect(harness.resource.getState().phase).toEqual({ tag: 'offline' });
        fixture.emit({ type: 'reconnected' });
        fixture.emit({ type: 'reconnected' });
        await flushPromises();

        expect(harness.acquireTransport).toHaveBeenCalledTimes(1);
        expect(fixture.transport.subscribe).toHaveBeenCalledTimes(1);
        expect(harness.protocol.create).toHaveBeenCalledTimes(2);

        secondCreate.resolve(created('after-reconnect', [{ id: 'second' }], 2));
        await flushPromises();
        expect(harness.resource.getState().phase).toMatchObject({
            tag: 'live',
            subscriptionId: 'after-reconnect',
        });

        await harness.resource.close();
    });

    test('the attached listener decodes server messages into machine events', async () => {
        const fixture = fakeTransport();
        const replacement = [{ id: 'replacement' }];
        const decodeMessage = (message, state) => {
            if (message.tag !== 'DidReplaceDataSubscription' || state.phase.tag !== 'live') {
                return null;
            }
            return {
                type: 'SERVER_SNAPSHOT',
                generation: state.generation,
                subscriptionId: state.phase.subscriptionId,
                value: message.value,
                revision: message.revision,
            };
        };
        const harness = makeHarness({
            transports: [fixture],
            create: async () => created('decoded-subscription', [{ id: 'initial' }], 1),
            decodeMessage,
        });

        harness.resource.updateDemand(1, false);
        await flushPromises();
        const message = {
            tag: 'DidReplaceDataSubscription',
            value: replacement,
            revision: 2,
        };
        fixture.emit({ type: 'message', message });

        expect(harness.protocol.decodeMessage).toHaveBeenCalledTimes(1);
        expect(harness.protocol.decodeMessage.mock.calls[0][0]).toBe(message);
        expect(harness.resource.getSnapshot().data).toBe(replacement);
        expect(harness.cacheWrites.at(-1)).toBe(replacement);
        expect(harness.resource.getState().phase).toMatchObject({ revision: 2 });

        await harness.resource.close();
    });

    test('reentrant publication dispatch is processed in FIFO order', async () => {
        const order = [];
        const reentrant = [{ id: 'reentrant' }];
        let dispatched = false;
        const harness = makeHarness({
            create: async () => created('reentrant-subscription', [{ id: 'initial' }], 1),
            publish: (_publication, state, _previousState, resource) => {
                if (state.phase.tag !== 'live') {
                    return;
                }
                order.push(`publish:${state.snapshot.data[0].id}`);
                if (!dispatched) {
                    dispatched = true;
                    resource.dispatchCompatibilityValue(reentrant);
                }
            },
        });
        const originalWriteCache = harness.cacheWrites.push.bind(harness.cacheWrites);
        harness.cacheWrites.push = value => {
            order.push(`cache:${value[0].id}`);
            return originalWriteCache(value);
        };

        harness.resource.updateDemand(1, false);
        await flushPromises();

        expect(order).toEqual([
            'cache:initial',
            'publish:initial',
            'cache:reentrant',
            'publish:reentrant',
        ]);
        expect(harness.resource.getSnapshot().data).toBe(reentrant);

        await harness.resource.close();
    });

    test('close awaits deletion of a live remote subscription', async () => {
        const deleteResult = deferred();
        const harness = makeHarness({
            create: async () => created('live-to-delete', [{ id: 'live' }]),
            deleteSubscription: () => deleteResult.promise,
        });

        harness.resource.updateDemand(1, false);
        await flushPromises();
        expect(harness.resource.getState().phase.tag).toBe('live');

        let closed = false;
        const closing = harness.resource.close().then(() => {
            closed = true;
        });
        await flushPromises();

        expect(harness.protocol.delete).toHaveBeenCalledWith(
            expect.any(Object),
            'live-to-delete',
        );
        expect(closed).toBe(false);

        deleteResult.resolve();
        await closing;
        expect(closed).toBe(true);
    });

    test('a reentrant close during publication still awaits its remote delete', async () => {
        const deleteResult = deferred();
        let closing;
        let closed = false;
        const harness = makeHarness({
            create: async () => created('reentrant-delete', [{ id: 'live' }]),
            deleteSubscription: () => deleteResult.promise,
            publish: (_publication, state, _previousState, resource) => {
                if (state.phase.tag === 'live' && closing === undefined) {
                    closing = resource.close().then(() => {
                        closed = true;
                    });
                }
            },
        });

        harness.resource.updateDemand(1, false);
        await flushPromises();

        expect(harness.protocol.delete).toHaveBeenCalledWith(
            expect.any(Object),
            'reentrant-delete',
        );
        expect(closed).toBe(false);

        deleteResult.resolve();
        await closing;
        expect(closed).toBe(true);
    });
});
