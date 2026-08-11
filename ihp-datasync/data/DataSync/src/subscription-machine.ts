import {
    initialResourceSnapshot,
    reduceResourceSnapshot,
    type ResourceSnapshot,
} from './subscription-reducer.js';
import type { UUID } from './types.js';

/** The server protocol used by a live subscription generation. */
export type SubscriptionMode = 'legacy' | 'snapshot';

/**
 * Immutable lifecycle state for a server-backed value.
 *
 * Effectful values such as controllers, promises, timers, caches and listener
 * functions deliberately do not belong here. A runtime closure interprets the
 * returned commands and feeds their results back as events.
 */
export type SubscriptionState<T> = Readonly<{
    scope: 'current' | 'stale';
    demand: Readonly<{
        subscribers: number;
        imperative: boolean;
    }>;
    generation: number;
    phase: SubscriptionPhase;
    snapshot: ResourceSnapshot<T>;
    refresh: 'clean' | 'running' | 'dirty';
    disposalToken: number;
    initialResult: 'pending' | 'settled';
}>;

export type SubscriptionPhase =
    | Readonly<{ tag: 'idle' }>
    | Readonly<{ tag: 'creating'; reconnect: boolean }>
    | Readonly<{
        tag: 'live';
        subscriptionId: UUID;
        mode: SubscriptionMode;
        revision: number;
    }>
    | Readonly<{ tag: 'offline' }>
    | Readonly<{ tag: 'failed' }>
    | Readonly<{ tag: 'closed' }>;

/** Resource-specific differences which should be explicit, not duplicated. */
export type SubscriptionPolicy = Readonly<{
    clearDataOnDisconnect: boolean;
    refreshLegacyInvalidations: boolean;
    cacheSnapshots: boolean;
    publishLegacyOnDisconnect: boolean;
    publishLegacyOnFailure: boolean;
    initialCloseMessage: string;
    initialUnusedMessage: string;
    scopeRetiredMessage: string;
}>;

/** Row subscriptions retain their last confirmed value while reconnecting. */
export const rowSubscriptionPolicy: SubscriptionPolicy = Object.freeze({
    clearDataOnDisconnect: false,
    refreshLegacyInvalidations: true,
    cacheSnapshots: true,
    publishLegacyOnDisconnect: false,
    publishLegacyOnFailure: true,
    initialCloseMessage: 'DataSubscription closed before its initial server snapshot arrived',
    initialUnusedMessage: 'DataSubscription became unused before its initial server snapshot arrived',
    scopeRetiredMessage: 'DataSubscription closed because its authentication/backend scope changed',
});

/** Count subscriptions historically clear and publish on disconnect. */
export const countSubscriptionPolicy: SubscriptionPolicy = Object.freeze({
    clearDataOnDisconnect: true,
    refreshLegacyInvalidations: false,
    cacheSnapshots: false,
    publishLegacyOnDisconnect: true,
    publishLegacyOnFailure: false,
    initialCloseMessage: 'CountSubscription closed before its initial server value arrived',
    initialUnusedMessage: 'CountSubscription became unused before its initial server value arrived',
    scopeRetiredMessage: 'CountSubscription closed because its authentication/backend scope changed',
});

export type CreatedSubscription<T> = Readonly<{
    subscriptionId: UUID;
    value: T;
    mode: SubscriptionMode;
    revision: number;
}>;

export type SubscriptionEvent<T> =
    | Readonly<{
        type: 'DEMAND_CHANGED';
        subscribers: number;
        imperative: boolean;
    }>
    | Readonly<{ type: 'SCHEDULE_IDLE_CHECK' }>
    | Readonly<{ type: 'IDLE_CHECK'; token: number }>
    | Readonly<{
        type: 'CREATE_SUCCEEDED';
        generation: number;
        created: CreatedSubscription<T>;
    }>
    | Readonly<{
        type: 'CREATE_FAILED';
        generation: number;
        error: Error;
    }>
    | Readonly<{
        type: 'SERVER_SNAPSHOT';
        generation: number;
        subscriptionId: UUID;
        value: T;
        revision: number;
    }>
    | Readonly<{
        /** Ordered, unrevisioned server value, primarily used by counts. */
        type: 'SERVER_VALUE';
        generation: number;
        subscriptionId: UUID;
        value: T;
    }>
    | Readonly<{
        type: 'LEGACY_INVALIDATED';
        generation: number;
        subscriptionId: UUID;
    }>
    | Readonly<{
        type: 'REFRESH_SUCCEEDED';
        generation: number;
        value: T;
    }>
    | Readonly<{
        type: 'REFRESH_FAILED';
        generation: number;
        error: Error;
    }>
    | Readonly<{ type: 'TRANSPORT_CLOSED' }>
    | Readonly<{ type: 'TRANSPORT_RECONNECTED' }>
    | Readonly<{ type: 'SCOPE_RETIRED' }>
    | Readonly<{ type: 'CLOSE_REQUESTED' }>
    | Readonly<{ type: 'COMPAT_VALUE_SET'; value: T | null }>;

export type SubscriptionCommand<T> =
    | Readonly<{
        type: 'CREATE_REMOTE';
        generation: number;
        reconnect: boolean;
    }>
    | Readonly<{
        type: 'DELETE_REMOTE';
        generation: number;
        subscriptionId: UUID;
    }>
    | Readonly<{ type: 'REFRESH_REMOTE'; generation: number }>
    | Readonly<{ type: 'QUEUE_IDLE_CHECK'; token: number }>
    | Readonly<{ type: 'CACHE_WRITE'; value: T }>
    | Readonly<{
        type: 'SETTLE_INITIAL';
        result:
            | Readonly<{ ok: true; value: T }>
            | Readonly<{ ok: false; error: Error }>;
    }>
    | Readonly<{ type: 'REPORT_REFRESH_FAILURE'; error: Error }>
    | Readonly<{ type: 'EVICT_FROM_REGISTRY' }>;

/** Which observer groups the imperative shell should notify. */
export type SubscriptionPublication = 'none' | 'snapshot' | 'legacy' | 'both';

export type SubscriptionTransition<T> = Readonly<{
    state: SubscriptionState<T>;
    commands: readonly SubscriptionCommand<T>[];
    publication: SubscriptionPublication;
}>;

const noCommands = Object.freeze([]) as readonly SubscriptionCommand<never>[];

/** Creates the frozen starting state, optionally seeded by a scoped cache. */
export function initialSubscriptionState<T>(
    initialData?: T | null,
): SubscriptionState<T> {
    return freezeState({
        scope: 'current',
        demand: { subscribers: 0, imperative: false },
        generation: 0,
        phase: { tag: 'idle' },
        snapshot: initialResourceSnapshot(initialData),
        refresh: 'clean',
        disposalToken: 0,
        initialResult: 'pending',
    });
}

/**
 * Pure transition function for row and count subscriptions.
 *
 * The runtime must install `transition.state` before interpreting commands.
 * Async command completions must be represented by another event carrying the
 * command's generation. Publications happen after synchronous commands such as
 * cache writes and initial-promise settlement have been interpreted.
 */
export function transitionSubscription<T>(
    state: SubscriptionState<T>,
    event: SubscriptionEvent<T>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    switch (event.type) {
        case 'DEMAND_CHANGED':
            return demandChanged(state, event, policy);
        case 'SCHEDULE_IDLE_CHECK':
            return scheduleIdleCheck(state);
        case 'IDLE_CHECK':
            return idleCheck(state, event.token, policy);
        case 'CREATE_SUCCEEDED':
            return createSucceeded(state, event, policy);
        case 'CREATE_FAILED':
            return createFailed(state, event, policy);
        case 'SERVER_SNAPSHOT':
            return serverSnapshot(state, event, policy);
        case 'SERVER_VALUE':
            return serverValue(state, event, policy);
        case 'LEGACY_INVALIDATED':
            return legacyInvalidated(state, event, policy);
        case 'REFRESH_SUCCEEDED':
            return refreshSucceeded(state, event, policy);
        case 'REFRESH_FAILED':
            return refreshFailed(state, event);
        case 'TRANSPORT_CLOSED':
            return transportClosed(state, policy);
        case 'TRANSPORT_RECONNECTED':
            return transportReconnected(state);
        case 'SCOPE_RETIRED':
            return scopeRetired(state, policy);
        case 'CLOSE_REQUESTED':
            return closeRequested(state, policy);
        case 'COMPAT_VALUE_SET':
            return compatValueSet(state, event.value, policy);
    }
}

function scheduleIdleCheck<T>(
    state: SubscriptionState<T>,
): SubscriptionTransition<T> {
    const disposalToken = state.disposalToken + 1;
    return changed(
        updateState(state, { disposalToken }),
        [{ type: 'QUEUE_IDLE_CHECK', token: disposalToken }],
    );
}

function compatValueSet<T>(
    state: SubscriptionState<T>,
    value: T | null,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    const snapshot = value === null
        ? initialResourceSnapshot<T>()
        : reduceResourceSnapshot(state.snapshot, { type: 'SNAPSHOT', data: value });
    const phase = value !== null && state.phase.tag === 'live'
        ? { ...state.phase, revision: state.phase.revision + 1 }
        : state.phase;
    const commands: SubscriptionCommand<T>[] = value !== null && policy.cacheSnapshots
        ? [{ type: 'CACHE_WRITE', value }]
        : [];
    return changed(
        updateState(state, { phase, snapshot }),
        commands,
        value === null || snapshot !== state.snapshot ? 'both' : 'none',
    );
}

function demandChanged<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'DEMAND_CHANGED' }>,
    _policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (!Number.isSafeInteger(event.subscribers) || event.subscribers < 0) {
        throw new RangeError('Subscription demand must be a non-negative safe integer');
    }

    if (
        state.demand.subscribers === event.subscribers
        && state.demand.imperative === event.imperative
    ) {
        return unchanged(state);
    }

    const wasDemanded = hasDemand(state.demand);
    const demand = { subscribers: event.subscribers, imperative: event.imperative };
    const isDemanded = hasDemand(demand);
    const demandBoundaryChanged = wasDemanded !== isDemanded;
    const updated = updateState(state, {
        demand,
        disposalToken: demandBoundaryChanged
            ? state.disposalToken + 1
            : state.disposalToken,
    });

    if (!wasDemanded && isDemanded) {
        if (updated.scope === 'stale') {
            return changed(updated);
        }
        switch (updated.phase.tag) {
            case 'idle':
            case 'closed':
            case 'failed':
                return beginCreate(updated, false);
            case 'offline':
                return beginCreate(updated, true);
            case 'creating':
            case 'live':
                return changed(updated);
        }
    }

    if (wasDemanded && !isDemanded) {
        return changed(updated, [{
            type: 'QUEUE_IDLE_CHECK',
            token: updated.disposalToken,
        }]);
    }

    return changed(updated);
}

function idleCheck<T>(
    state: SubscriptionState<T>,
    token: number,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (
        token !== state.disposalToken
        || hasDemand(state.demand)
        || state.phase.tag === 'closed'
    ) {
        return unchanged(state);
    }
    return closeCurrent(state, false, policy);
}

function createSucceeded<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'CREATE_SUCCEEDED' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    const { created } = event;
    const isCurrentCreate = state.scope === 'current'
        && state.phase.tag === 'creating'
        && event.generation === state.generation;

    if (!isCurrentCreate) {
        if (
            state.phase.tag === 'live'
            && event.generation === state.generation
            && state.phase.subscriptionId === created.subscriptionId
        ) {
            return unchanged(state);
        }
        return changed(state, [{
            type: 'DELETE_REMOTE',
            generation: event.generation,
            subscriptionId: created.subscriptionId,
        }]);
    }

    const revision = created.mode === 'snapshot' ? created.revision : 0;
    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'SNAPSHOT',
        data: created.value,
    });
    const shouldSettleInitial = state.initialResult === 'pending';
    const nextState = updateState(state, {
        phase: {
            tag: 'live',
            subscriptionId: created.subscriptionId,
            mode: created.mode,
            revision,
        },
        snapshot,
        refresh: 'clean',
        initialResult: shouldSettleInitial ? 'settled' : state.initialResult,
    });
    const commands: SubscriptionCommand<T>[] = [];
    if (policy.cacheSnapshots) {
        commands.push({ type: 'CACHE_WRITE', value: created.value });
    }
    if (shouldSettleInitial) {
        commands.push({
            type: 'SETTLE_INITIAL',
            result: { ok: true, value: created.value },
        });
    }
    return changed(
        nextState,
        commands,
        snapshot === state.snapshot ? 'none' : 'both',
    );
}

function createFailed<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'CREATE_FAILED' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (
        state.scope !== 'current'
        || state.phase.tag !== 'creating'
        || event.generation !== state.generation
    ) {
        return unchanged(state);
    }

    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'FAIL',
        error: event.error,
    });
    const shouldSettleInitial = state.initialResult === 'pending';
    const nextState = updateState(state, {
        phase: { tag: 'failed' },
        snapshot,
        refresh: 'clean',
        initialResult: shouldSettleInitial ? 'settled' : state.initialResult,
    });
    const commands: SubscriptionCommand<T>[] = shouldSettleInitial
        ? [{
            type: 'SETTLE_INITIAL',
            result: { ok: false, error: event.error },
        }]
        : [];
    return changed(
        nextState,
        commands,
        snapshot === state.snapshot
            ? 'none'
            : policy.publishLegacyOnFailure ? 'both' : 'snapshot',
    );
}

function serverSnapshot<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'SERVER_SNAPSHOT' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (!isCurrentLiveEvent(state, event.generation, event.subscriptionId)) {
        return unchanged(state);
    }
    const live = state.phase;
    if (live.tag !== 'live') {
        return unchanged(state);
    }
    if (live.mode === 'snapshot' && event.revision <= live.revision) {
        return unchanged(state);
    }

    return acceptServerValue(
        state,
        event.value,
        {
            tag: 'live',
            subscriptionId: live.subscriptionId,
            mode: 'snapshot',
            revision: event.revision,
        },
        'clean',
        policy,
    );
}

function serverValue<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'SERVER_VALUE' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (!isCurrentLiveEvent(state, event.generation, event.subscriptionId)) {
        return unchanged(state);
    }
    const live = state.phase;
    if (live.tag !== 'live' || live.mode !== 'legacy') {
        return unchanged(state);
    }
    return acceptServerValue(
        state,
        event.value,
        { ...live, revision: live.revision + 1 },
        state.refresh,
        policy,
    );
}

function legacyInvalidated<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'LEGACY_INVALIDATED' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (
        !policy.refreshLegacyInvalidations
        || !isCurrentLiveEvent(state, event.generation, event.subscriptionId)
        || state.phase.tag !== 'live'
        || state.phase.mode !== 'legacy'
    ) {
        return unchanged(state);
    }
    switch (state.refresh) {
        case 'clean':
            return changed(
                updateState(state, { refresh: 'running' }),
                [{ type: 'REFRESH_REMOTE', generation: state.generation }],
            );
        case 'running':
            return changed(updateState(state, { refresh: 'dirty' }));
        case 'dirty':
            return unchanged(state);
    }
}

function refreshSucceeded<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'REFRESH_SUCCEEDED' }>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (
        state.scope !== 'current'
        || event.generation !== state.generation
        || state.phase.tag !== 'live'
        || state.phase.mode !== 'legacy'
        || state.refresh === 'clean'
    ) {
        return unchanged(state);
    }

    const wasDirty = state.refresh === 'dirty';
    const commands: SubscriptionCommand<T>[] = [];
    if (policy.cacheSnapshots) {
        commands.push({ type: 'CACHE_WRITE', value: event.value });
    }
    if (wasDirty) {
        commands.push({ type: 'REFRESH_REMOTE', generation: state.generation });
    }
    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'SNAPSHOT',
        data: event.value,
    });
    const nextState = updateState(state, {
        phase: { ...state.phase, revision: state.phase.revision + 1 },
        snapshot,
        refresh: wasDirty ? 'running' : 'clean',
    });
    return changed(
        nextState,
        commands,
        snapshot === state.snapshot ? 'none' : 'both',
    );
}

function refreshFailed<T>(
    state: SubscriptionState<T>,
    event: Extract<SubscriptionEvent<T>, { type: 'REFRESH_FAILED' }>,
): SubscriptionTransition<T> {
    if (
        state.scope !== 'current'
        || event.generation !== state.generation
        || state.phase.tag !== 'live'
        || state.phase.mode !== 'legacy'
        || state.refresh === 'clean'
    ) {
        return unchanged(state);
    }
    const wasDirty = state.refresh === 'dirty';
    const commands: SubscriptionCommand<T>[] = [{
        type: 'REPORT_REFRESH_FAILURE',
        error: event.error,
    }];
    if (wasDirty) {
        commands.push({ type: 'REFRESH_REMOTE', generation: state.generation });
    }
    return changed(
        updateState(state, { refresh: wasDirty ? 'running' : 'clean' }),
        commands,
    );
}

function transportClosed<T>(
    state: SubscriptionState<T>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (
        state.scope === 'stale'
        || state.phase.tag === 'idle'
        || state.phase.tag === 'closed'
        || state.phase.tag === 'offline'
    ) {
        return unchanged(state);
    }
    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'DISCONNECT',
        clearData: policy.clearDataOnDisconnect,
    });
    const nextState = updateState(state, {
        generation: state.generation + 1,
        phase: { tag: 'offline' },
        snapshot,
        refresh: 'clean',
    });
    return changed(
        nextState,
        [],
        snapshot === state.snapshot
            ? 'none'
            : policy.publishLegacyOnDisconnect ? 'both' : 'snapshot',
    );
}

function transportReconnected<T>(
    state: SubscriptionState<T>,
): SubscriptionTransition<T> {
    if (
        state.scope !== 'current'
        || !hasDemand(state.demand)
        || (state.phase.tag !== 'offline' && state.phase.tag !== 'failed')
    ) {
        return unchanged(state);
    }
    return beginCreate(state, true);
}

function scopeRetired<T>(
    state: SubscriptionState<T>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    if (state.scope === 'stale') {
        return unchanged(state);
    }
    const clearedSnapshot = reduceResourceSnapshot(
        initialResourceSnapshot<T>(),
        { type: 'CLOSE' },
    );
    const shouldSettleInitial = state.initialResult === 'pending';
    const error = new Error(policy.scopeRetiredMessage);
    const nextState = updateState(state, {
        scope: 'stale',
        generation: state.generation + 1,
        phase: { tag: 'closed' },
        snapshot: clearedSnapshot,
        refresh: 'clean',
        disposalToken: state.disposalToken + 1,
        initialResult: shouldSettleInitial ? 'settled' : state.initialResult,
    });
    const commands: SubscriptionCommand<T>[] = [];
    if (shouldSettleInitial) {
        commands.push({
            type: 'SETTLE_INITIAL',
            result: { ok: false, error },
        });
    }
    commands.push({ type: 'EVICT_FROM_REGISTRY' });
    return changed(
        nextState,
        commands,
        clearedSnapshot === state.snapshot ? 'none' : 'snapshot',
    );
}

function closeRequested<T>(
    state: SubscriptionState<T>,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    const demandWasEmpty = !hasDemand(state.demand);
    const demand = { subscribers: 0, imperative: false };
    if (
        state.scope === 'stale'
        || (state.phase.tag === 'closed' && demandWasEmpty)
    ) {
        if (demandWasEmpty) {
            return unchanged(state);
        }
        return changed(updateState(state, { demand }));
    }
    const withoutDemand = updateState(state, {
        demand,
        disposalToken: state.disposalToken + 1,
    });
    return closeCurrent(withoutDemand, true, policy);
}

function beginCreate<T>(
    state: SubscriptionState<T>,
    reconnect: boolean,
): SubscriptionTransition<T> {
    const generation = state.generation + 1;
    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'CONNECT',
        reconnect,
    });
    const nextState = updateState(state, {
        generation,
        phase: { tag: 'creating', reconnect },
        snapshot,
        refresh: 'clean',
    });
    return changed(
        nextState,
        [{ type: 'CREATE_REMOTE', generation, reconnect }],
        snapshot === state.snapshot ? 'none' : 'snapshot',
    );
}

function closeCurrent<T>(
    state: SubscriptionState<T>,
    explicit: boolean,
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    const commands: SubscriptionCommand<T>[] = [];
    if (state.phase.tag === 'live') {
        commands.push({
            type: 'DELETE_REMOTE',
            generation: state.generation,
            subscriptionId: state.phase.subscriptionId,
        });
    }

    const shouldSettleInitial = state.initialResult === 'pending';
    if (shouldSettleInitial) {
        commands.push({
            type: 'SETTLE_INITIAL',
            result: {
                ok: false,
                error: new Error(explicit
                    ? policy.initialCloseMessage
                    : policy.initialUnusedMessage),
            },
        });
    }
    commands.push({ type: 'EVICT_FROM_REGISTRY' });

    const snapshot = reduceResourceSnapshot(state.snapshot, { type: 'CLOSE' });
    const nextState = updateState(state, {
        generation: state.generation + 1,
        phase: { tag: 'closed' },
        snapshot,
        refresh: 'clean',
        initialResult: shouldSettleInitial ? 'settled' : state.initialResult,
    });
    return changed(
        nextState,
        commands,
        snapshot === state.snapshot ? 'none' : 'snapshot',
    );
}

function acceptServerValue<T>(
    state: SubscriptionState<T>,
    value: T,
    phase: Extract<SubscriptionPhase, { tag: 'live' }>,
    refresh: SubscriptionState<T>['refresh'],
    policy: SubscriptionPolicy,
): SubscriptionTransition<T> {
    const snapshot = reduceResourceSnapshot(state.snapshot, {
        type: 'SNAPSHOT',
        data: value,
    });
    const commands: SubscriptionCommand<T>[] = policy.cacheSnapshots
        ? [{ type: 'CACHE_WRITE', value }]
        : [];
    const nextState = updateState(state, { phase, snapshot, refresh });
    return changed(
        nextState,
        commands,
        snapshot === state.snapshot ? 'none' : 'both',
    );
}

function isCurrentLiveEvent<T>(
    state: SubscriptionState<T>,
    generation: number,
    subscriptionId: UUID,
): boolean {
    return state.scope === 'current'
        && state.generation === generation
        && state.phase.tag === 'live'
        && state.phase.subscriptionId === subscriptionId;
}

function hasDemand(demand: SubscriptionState<unknown>['demand']): boolean {
    return demand.subscribers > 0 || demand.imperative;
}

function updateState<T>(
    state: SubscriptionState<T>,
    patch: Partial<SubscriptionState<T>>,
): SubscriptionState<T> {
    return freezeState({ ...state, ...patch });
}

function freezeState<T>(state: SubscriptionState<T>): SubscriptionState<T> {
    return Object.freeze({
        ...state,
        demand: Object.freeze({ ...state.demand }),
        phase: Object.freeze({ ...state.phase }),
    });
}

function unchanged<T>(state: SubscriptionState<T>): SubscriptionTransition<T> {
    return Object.freeze({
        state,
        commands: noCommands as readonly SubscriptionCommand<T>[],
        publication: 'none' as const,
    });
}

function changed<T>(
    state: SubscriptionState<T>,
    commands: SubscriptionCommand<T>[] = [],
    publication: SubscriptionPublication = 'none',
): SubscriptionTransition<T> {
    const frozenCommands = commands.length === 0
        ? noCommands as readonly SubscriptionCommand<T>[]
        : Object.freeze(commands.map(freezeCommand));
    return Object.freeze({ state, commands: frozenCommands, publication });
}

function freezeCommand<T>(command: SubscriptionCommand<T>): SubscriptionCommand<T> {
    if (command.type === 'SETTLE_INITIAL') {
        return Object.freeze({
            ...command,
            result: Object.freeze({ ...command.result }),
        });
    }
    return Object.freeze({ ...command });
}
