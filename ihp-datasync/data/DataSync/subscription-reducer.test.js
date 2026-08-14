import {
    createIdleResourceSnapshot,
    idleResourceSnapshot,
    initialResourceSnapshot,
    reduceResourceSnapshot,
} from './subscription-reducer.js';

describe('reduceResourceSnapshot', () => {
    test('uses one immutable idle snapshot for every resource type', () => {
        const first = createIdleResourceSnapshot();
        const second = createIdleResourceSnapshot();

        expect(first).toBe(idleResourceSnapshot);
        expect(second).toBe(first);
        expect(initialResourceSnapshot()).toBe(first);
        expect(first).toEqual({ data: null, status: 'idle', error: null });
        expect(Object.isFrozen(first)).toBe(true);
    });

    test('can seed an idle snapshot from a server cache without marking it live', () => {
        const records = [{ id: 'cached-task' }];
        const snapshot = initialResourceSnapshot(records);

        expect(snapshot).toEqual({ data: records, status: 'idle', error: null });
        expect(snapshot.data).toBe(records);
        expect(snapshot).not.toBe(idleResourceSnapshot);
        expect(Object.isFrozen(snapshot)).toBe(true);
        expect(initialResourceSnapshot(null)).toBe(idleResourceSnapshot);
    });

    test('moves through the initial connection and live snapshot states', () => {
        const idle = createIdleResourceSnapshot();
        const connecting = reduceResourceSnapshot(idle, { type: 'CONNECT', reconnect: false });
        const records = [{ id: 'task-1' }];
        const live = reduceResourceSnapshot(connecting, { type: 'SNAPSHOT', data: records });

        expect(connecting).toEqual({ data: null, status: 'connecting', error: null });
        expect(live).toEqual({ data: records, status: 'live', error: null });
        expect(connecting).not.toBe(idle);
        expect(live).not.toBe(connecting);
        expect(Object.isFrozen(live)).toBe(true);
    });

    test('retains the last server snapshot while disconnected and reconnecting', () => {
        const records = [{ id: 'task-1' }];
        const live = reduceResourceSnapshot(createIdleResourceSnapshot(), { type: 'SNAPSHOT', data: records });
        const disconnected = reduceResourceSnapshot(live, { type: 'DISCONNECT' });
        const reconnecting = reduceResourceSnapshot(disconnected, { type: 'CONNECT', reconnect: true });

        expect(disconnected).toEqual({ data: records, status: 'reconnecting', error: null });
        expect(reconnecting.data).toBe(records);
        expect(reconnecting).toBe(disconnected);
    });

    test('retains data on failure and replaces the observable error', () => {
        const records = [{ id: 'task-1' }];
        const live = reduceResourceSnapshot(createIdleResourceSnapshot(), { type: 'SNAPSHOT', data: records });
        const firstError = new Error('first failure');
        const failed = reduceResourceSnapshot(live, { type: 'FAIL', error: firstError });
        const secondError = new Error('second failure');
        const failedAgain = reduceResourceSnapshot(failed, { type: 'FAIL', error: secondError });

        expect(failed).toEqual({ data: records, status: 'error', error: firstError });
        expect(failedAgain.data).toBe(records);
        expect(failedAgain.error).toBe(secondError);
        expect(failedAgain).not.toBe(failed);
    });

    test('keeps the previous identity for semantic no-ops', () => {
        const records = [{ id: 'task-1' }];
        const live = reduceResourceSnapshot(createIdleResourceSnapshot(), { type: 'SNAPSHOT', data: records });
        const error = new Error('failure');
        const failed = reduceResourceSnapshot(live, { type: 'FAIL', error });
        const reconnecting = reduceResourceSnapshot(live, { type: 'DISCONNECT' });
        const closed = reduceResourceSnapshot(live, { type: 'CLOSE' });

        expect(reduceResourceSnapshot(live, { type: 'SNAPSHOT', data: records })).toBe(live);
        expect(reduceResourceSnapshot(failed, { type: 'FAIL', error })).toBe(failed);
        expect(reduceResourceSnapshot(reconnecting, { type: 'DISCONNECT' })).toBe(reconnecting);
        expect(reduceResourceSnapshot(closed, { type: 'CLOSE' })).toBe(closed);
    });

    test('treats a new data reference as a new observable snapshot', () => {
        const records = [{ id: 'task-1' }];
        const live = reduceResourceSnapshot(createIdleResourceSnapshot(), { type: 'SNAPSHOT', data: records });
        const replacement = [{ id: 'task-1' }];
        const updated = reduceResourceSnapshot(live, { type: 'SNAPSHOT', data: replacement });

        expect(updated).not.toBe(live);
        expect(updated.data).toBe(replacement);
    });

    test('clears an error when connecting, receiving data, or closing', () => {
        const error = new Error('failure');
        const failed = reduceResourceSnapshot(createIdleResourceSnapshot(), { type: 'FAIL', error });
        const connecting = reduceResourceSnapshot(failed, { type: 'CONNECT', reconnect: false });
        const failedAgain = reduceResourceSnapshot(connecting, { type: 'FAIL', error });
        const live = reduceResourceSnapshot(failedAgain, { type: 'SNAPSHOT', data: 1 });
        const failedOnceMore = reduceResourceSnapshot(live, { type: 'FAIL', error });
        const closed = reduceResourceSnapshot(failedOnceMore, { type: 'CLOSE' });

        expect(connecting.error).toBeNull();
        expect(live.error).toBeNull();
        expect(closed).toEqual({ data: 1, status: 'closed', error: null });
    });
});
