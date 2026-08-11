import {
    countSubscriptionPolicy,
    initialSubscriptionState,
    rowSubscriptionPolicy,
    transitionSubscription,
} from './subscription-machine.js';

const demand = (state, subscribers, imperative = false, policy = rowSubscriptionPolicy) =>
    transitionSubscription(state, {
        type: 'DEMAND_CHANGED',
        subscribers,
        imperative,
    }, policy);

const create = (state, {
    id = 'subscription-1',
    value = [{ id: 'row-1' }],
    mode = 'snapshot',
    revision = 0,
} = {}, policy = rowSubscriptionPolicy) => transitionSubscription(state, {
    type: 'CREATE_SUCCEEDED',
    generation: state.generation,
    created: { subscriptionId: id, value, mode, revision },
}, policy);

describe('pure subscription machine', () => {
    test('an explicit idle check closes an unused idle resource and rejects its initial result', () => {
        const idle = initialSubscriptionState();
        const scheduled = transitionSubscription(idle, {
            type: 'SCHEDULE_IDLE_CHECK',
        }, rowSubscriptionPolicy);

        expect(scheduled.state.phase).toEqual({ tag: 'idle' });
        expect(scheduled.state.disposalToken).toBe(idle.disposalToken + 1);
        expect(scheduled.commands).toEqual([{
            type: 'QUEUE_IDLE_CHECK',
            token: scheduled.state.disposalToken,
        }]);

        const closed = transitionSubscription(scheduled.state, {
            type: 'IDLE_CHECK',
            token: scheduled.state.disposalToken,
        }, rowSubscriptionPolicy);

        expect(closed.state.phase).toEqual({ tag: 'closed' });
        expect(closed.state.initialResult).toBe('settled');
        expect(closed.state.snapshot.status).toBe('closed');
        expect(closed.commands).toHaveLength(2);
        expect(closed.commands[0]).toMatchObject({
            type: 'SETTLE_INITIAL',
            result: { ok: false },
        });
        expect(closed.commands[0].result.error.message)
            .toBe(rowSubscriptionPolicy.initialUnusedMessage);
        expect(closed.commands[1]).toEqual({ type: 'EVICT_FROM_REGISTRY' });
    });

    test('demand arriving before an explicit idle callback cancels its token', () => {
        const scheduled = transitionSubscription(initialSubscriptionState(), {
            type: 'SCHEDULE_IDLE_CHECK',
        }, rowSubscriptionPolicy);
        const scheduledToken = scheduled.state.disposalToken;
        const demanded = demand(scheduled.state, 1);

        expect(demanded.state.disposalToken).toBeGreaterThan(scheduledToken);
        expect(demanded.commands).toEqual([{
            type: 'CREATE_REMOTE',
            generation: 1,
            reconnect: false,
        }]);

        const staleCallback = transitionSubscription(demanded.state, {
            type: 'IDLE_CHECK',
            token: scheduledToken,
        }, rowSubscriptionPolicy);

        expect(staleCallback.state).toBe(demanded.state);
        expect(staleCallback.commands).toEqual([]);
        expect(staleCallback.state.phase).toEqual({ tag: 'creating', reconnect: false });
    });

    test('starts only on first demand and schedules disposal only after last demand', () => {
        const idle = initialSubscriptionState();
        const first = demand(idle, 1);

        expect(first.state.phase).toEqual({ tag: 'creating', reconnect: false });
        expect(first.state.snapshot.status).toBe('connecting');
        expect(first.commands).toEqual([{
            type: 'CREATE_REMOTE',
            generation: 1,
            reconnect: false,
        }]);

        const second = demand(first.state, 2);
        expect(second.commands).toEqual([]);
        expect(second.state.generation).toBe(1);

        const oneRemaining = demand(second.state, 1);
        expect(oneRemaining.commands).toEqual([]);

        const released = demand(oneRemaining.state, 0);
        expect(released.commands).toEqual([{
            type: 'QUEUE_IDLE_CHECK',
            token: released.state.disposalToken,
        }]);
        expect(released.state.phase.tag).toBe('creating');
    });

    test('a new demand cancels an already queued idle check by token identity', () => {
        const started = demand(initialSubscriptionState(), 1).state;
        const released = demand(started, 0).state;
        const oldToken = released.disposalToken;
        const retainedAgain = demand(released, 1).state;

        expect(retainedAgain.disposalToken).toBeGreaterThan(oldToken);
        const staleCheck = transitionSubscription(retainedAgain, {
            type: 'IDLE_CHECK',
            token: oldToken,
        }, rowSubscriptionPolicy);

        expect(staleCheck.state).toBe(retainedAgain);
        expect(staleCheck.commands).toEqual([]);
    });

    test('the matching idle check closes and a late create is deleted without publication', () => {
        const creating = demand(initialSubscriptionState(), 1).state;
        const generation = creating.generation;
        const released = demand(creating, 0).state;
        const closed = transitionSubscription(released, {
            type: 'IDLE_CHECK',
            token: released.disposalToken,
        }, rowSubscriptionPolicy);

        expect(closed.state.phase.tag).toBe('closed');
        expect(closed.commands.map(command => command.type)).toEqual([
            'SETTLE_INITIAL',
            'EVICT_FROM_REGISTRY',
        ]);

        const late = transitionSubscription(closed.state, {
            type: 'CREATE_SUCCEEDED',
            generation,
            created: {
                subscriptionId: 'late-subscription',
                value: [{ id: 'secret' }],
                mode: 'snapshot',
                revision: 0,
            },
        }, rowSubscriptionPolicy);

        expect(late.state).toBe(closed.state);
        expect(late.publication).toBe('none');
        expect(late.commands).toEqual([{
            type: 'DELETE_REMOTE',
            generation,
            subscriptionId: 'late-subscription',
        }]);
        expect(late.state.snapshot.data).toBeNull();
    });

    test('accepts only increasing snapshot revisions and preserves no-op identity', () => {
        const live = create(demand(initialSubscriptionState(), 1).state, {
            revision: 4,
        }).state;
        const previousSnapshot = live.snapshot;

        const duplicate = transitionSubscription(live, {
            type: 'SERVER_SNAPSHOT',
            generation: live.generation,
            subscriptionId: 'subscription-1',
            value: [{ id: 'duplicate' }],
            revision: 4,
        }, rowSubscriptionPolicy);
        const older = transitionSubscription(live, {
            type: 'SERVER_SNAPSHOT',
            generation: live.generation,
            subscriptionId: 'subscription-1',
            value: [{ id: 'older' }],
            revision: 3,
        }, rowSubscriptionPolicy);

        expect(duplicate.state).toBe(live);
        expect(older.state).toBe(live);
        expect(duplicate.state.snapshot).toBe(previousSnapshot);

        const replacement = [{ id: 'newer' }];
        const newer = transitionSubscription(live, {
            type: 'SERVER_SNAPSHOT',
            generation: live.generation,
            subscriptionId: 'subscription-1',
            value: replacement,
            revision: 5,
        }, rowSubscriptionPolicy);

        expect(newer.state).not.toBe(live);
        expect(newer.state.snapshot.data).toBe(replacement);
        expect(newer.state.phase).toEqual({
            tag: 'live',
            subscriptionId: 'subscription-1',
            mode: 'snapshot',
            revision: 5,
        });
    });

    test('coalesces legacy invalidations into at most one follow-up refresh', () => {
        const live = create(demand(initialSubscriptionState(), 1).state, {
            mode: 'legacy',
        }).state;
        const first = transitionSubscription(live, {
            type: 'LEGACY_INVALIDATED',
            generation: live.generation,
            subscriptionId: 'subscription-1',
        }, rowSubscriptionPolicy);
        const second = transitionSubscription(first.state, {
            type: 'LEGACY_INVALIDATED',
            generation: live.generation,
            subscriptionId: 'subscription-1',
        }, rowSubscriptionPolicy);
        const third = transitionSubscription(second.state, {
            type: 'LEGACY_INVALIDATED',
            generation: live.generation,
            subscriptionId: 'subscription-1',
        }, rowSubscriptionPolicy);

        expect(first.state.refresh).toBe('running');
        expect(first.commands).toEqual([{
            type: 'REFRESH_REMOTE',
            generation: live.generation,
        }]);
        expect(second.state.refresh).toBe('dirty');
        expect(second.commands).toEqual([]);
        expect(third.state).toBe(second.state);

        const refreshed = transitionSubscription(second.state, {
            type: 'REFRESH_SUCCEEDED',
            generation: live.generation,
            value: [{ id: 'refreshed' }],
        }, rowSubscriptionPolicy);
        expect(refreshed.state.refresh).toBe('running');
        expect(refreshed.commands.map(command => command.type)).toEqual([
            'CACHE_WRITE',
            'REFRESH_REMOTE',
        ]);
    });

    test('a revisioned snapshot upgrade makes an in-flight legacy refresh stale', () => {
        const live = create(demand(initialSubscriptionState(), 1).state, {
            mode: 'legacy',
        }).state;
        const refreshing = transitionSubscription(live, {
            type: 'LEGACY_INVALIDATED',
            generation: live.generation,
            subscriptionId: 'subscription-1',
        }, rowSubscriptionPolicy).state;
        const upgraded = transitionSubscription(refreshing, {
            type: 'SERVER_SNAPSHOT',
            generation: live.generation,
            subscriptionId: 'subscription-1',
            value: [{ id: 'v2' }],
            revision: 0,
        }, rowSubscriptionPolicy);

        expect(upgraded.state.refresh).toBe('clean');
        expect(upgraded.state.phase).toMatchObject({ mode: 'snapshot', revision: 0 });

        const staleRefresh = transitionSubscription(upgraded.state, {
            type: 'REFRESH_SUCCEEDED',
            generation: live.generation,
            value: [{ id: 'stale-legacy' }],
        }, rowSubscriptionPolicy);
        expect(staleRefresh.state).toBe(upgraded.state);
        expect(staleRefresh.state.snapshot.data).toEqual([{ id: 'v2' }]);
    });

    test('network disconnect retains rows, clears counts, and reconnects demanded resources', () => {
        const rows = [{ id: 'row-1' }];
        const rowLive = create(demand(initialSubscriptionState(), 1).state, {
            value: rows,
        }).state;
        const rowOffline = transitionSubscription(rowLive, {
            type: 'TRANSPORT_CLOSED',
        }, rowSubscriptionPolicy);

        expect(rowOffline.state.snapshot).toEqual({
            data: rows,
            status: 'reconnecting',
            error: null,
        });
        expect(rowOffline.publication).toBe('snapshot');

        const countLive = create(
            demand(initialSubscriptionState(), 1, false, countSubscriptionPolicy).state,
            { value: 3, mode: 'legacy' },
            countSubscriptionPolicy,
        ).state;
        const countOffline = transitionSubscription(countLive, {
            type: 'TRANSPORT_CLOSED',
        }, countSubscriptionPolicy);

        expect(countOffline.state.snapshot.data).toBeNull();
        expect(countOffline.publication).toBe('both');

        const reconnecting = transitionSubscription(rowOffline.state, {
            type: 'TRANSPORT_RECONNECTED',
        }, rowSubscriptionPolicy);
        expect(reconnecting.state.phase).toEqual({ tag: 'creating', reconnect: true });
        expect(reconnecting.commands).toEqual([{
            type: 'CREATE_REMOTE',
            generation: reconnecting.state.generation,
            reconnect: true,
        }]);
    });

    test('scope retirement clears security-sensitive data and rejects an unsettled initial result', () => {
        const cached = [{ id: 'old-user-secret' }];
        const creating = demand(initialSubscriptionState(cached), 1).state;
        const retired = transitionSubscription(creating, {
            type: 'SCOPE_RETIRED',
        }, rowSubscriptionPolicy);

        expect(retired.state.scope).toBe('stale');
        expect(retired.state.phase.tag).toBe('closed');
        expect(retired.state.snapshot).toEqual({
            data: null,
            status: 'closed',
            error: null,
        });
        expect(retired.commands.map(command => command.type)).toEqual([
            'SETTLE_INITIAL',
            'EVICT_FROM_REGISTRY',
        ]);
        expect(retired.commands[0].result.ok).toBe(false);
    });

    test('successful and failed creates settle the initial result exactly once', () => {
        const creating = demand(initialSubscriptionState(), 1).state;
        const value = [{ id: 'row-1' }];
        const succeeded = create(creating, { value });

        expect(succeeded.state.initialResult).toBe('settled');
        expect(succeeded.commands).toEqual([
            { type: 'CACHE_WRITE', value },
            { type: 'SETTLE_INITIAL', result: { ok: true, value } },
        ]);

        const replacement = transitionSubscription(succeeded.state, {
            type: 'SERVER_SNAPSHOT',
            generation: succeeded.state.generation,
            subscriptionId: 'subscription-1',
            value: [{ id: 'row-2' }],
            revision: 1,
        }, rowSubscriptionPolicy);
        expect(replacement.commands.some(command => command.type === 'SETTLE_INITIAL')).toBe(false);

        const otherCreating = demand(initialSubscriptionState(), 1).state;
        const error = new Error('create failed');
        const failed = transitionSubscription(otherCreating, {
            type: 'CREATE_FAILED',
            generation: otherCreating.generation,
            error,
        }, rowSubscriptionPolicy);
        expect(failed.state.initialResult).toBe('settled');
        expect(failed.commands).toEqual([{
            type: 'SETTLE_INITIAL',
            result: { ok: false, error },
        }]);
    });

    test('never mutates prior state or snapshot wrappers', () => {
        const idle = initialSubscriptionState([{ id: 'cached' }]);
        const original = {
            generation: idle.generation,
            phase: idle.phase,
            snapshot: idle.snapshot,
            demand: idle.demand,
        };
        const started = demand(idle, 1);

        expect(idle.generation).toBe(original.generation);
        expect(idle.phase).toBe(original.phase);
        expect(idle.snapshot).toBe(original.snapshot);
        expect(idle.demand).toBe(original.demand);
        expect(Object.isFrozen(idle)).toBe(true);
        expect(Object.isFrozen(idle.phase)).toBe(true);
        expect(Object.isFrozen(idle.demand)).toBe(true);
        expect(Object.isFrozen(idle.snapshot)).toBe(true);
        expect(Object.isFrozen(started.state)).toBe(true);
        expect(Object.isFrozen(started.commands)).toBe(true);
        expect(Object.isFrozen(started.commands[0])).toBe(true);
    });

    test('models legacy writable value facades without changing transport ownership', () => {
        const idle = initialSubscriptionState();
        const redundantClear = transitionSubscription(idle, {
            type: 'COMPAT_VALUE_SET',
            value: null,
        }, rowSubscriptionPolicy);
        expect(redundantClear.state.snapshot).toBe(idle.snapshot);
        expect(redundantClear.publication).toBe('both');

        const assigned = transitionSubscription(idle, {
            type: 'COMPAT_VALUE_SET',
            value: [{ id: 'manual' }],
        }, rowSubscriptionPolicy);

        expect(assigned.state.phase).toEqual(idle.phase);
        expect(assigned.state.snapshot).toEqual({
            data: [{ id: 'manual' }],
            status: 'live',
            error: null,
        });
        expect(assigned.commands).toEqual([{
            type: 'CACHE_WRITE',
            value: [{ id: 'manual' }],
        }]);
        expect(assigned.publication).toBe('both');

        const cleared = transitionSubscription(assigned.state, {
            type: 'COMPAT_VALUE_SET',
            value: null,
        }, rowSubscriptionPolicy);
        expect(cleared.state.phase).toEqual(idle.phase);
        expect(cleared.state.snapshot).toEqual({
            data: null,
            status: 'idle',
            error: null,
        });
    });
});
