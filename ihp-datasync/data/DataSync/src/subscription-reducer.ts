/** The observable lifecycle of a server-backed resource. */
export type ResourceStatus =
    | 'idle'
    | 'connecting'
    | 'live'
    | 'reconnecting'
    | 'error'
    | 'closed';

/**
 * An immutable value exposed by a server-backed external store.
 *
 * `data` is normally retained while reconnecting or after an error so a
 * consumer can continue to render the last confirmed server snapshot. A
 * resource whose legacy contract cleared on disconnect can request that
 * resource-specific transition explicitly.
 */
export type ResourceSnapshot<T> = Readonly<{
    data: T | null;
    status: ResourceStatus;
    error: Error | null;
}>;

export type ResourceSnapshotAction<T> =
    | Readonly<{ type: 'CONNECT'; reconnect: boolean }>
    | Readonly<{ type: 'SNAPSHOT'; data: T }>
    | Readonly<{ type: 'DISCONNECT'; clearData?: boolean }>
    | Readonly<{ type: 'FAIL'; error: Error }>
    | Readonly<{ type: 'CLOSE' }>;

/** Shared initial snapshot. It is safe to reuse because snapshots are immutable. */
export const idleResourceSnapshot: ResourceSnapshot<never> = Object.freeze({
    data: null,
    status: 'idle',
    error: null,
});

/**
 * Creates an idle snapshot, optionally seeded with a cached server value.
 * Empty snapshots share one object; seeded snapshots are frozen individually.
 */
export function initialResourceSnapshot<T>(data?: T | null): ResourceSnapshot<T> {
    if (data === null || data === undefined) {
        return idleResourceSnapshot;
    }

    return Object.freeze({
        data,
        status: 'idle',
        error: null,
    });
}

/** @deprecated Prefer the lifecycle-oriented `initialResourceSnapshot` name. */
export const createIdleResourceSnapshot = initialResourceSnapshot;

/**
 * Applies a resource lifecycle event without mutating the previous snapshot.
 * Semantic no-ops return the previous object so `useSyncExternalStore` readers
 * can rely on referential stability.
 */
export function reduceResourceSnapshot<T>(
    snapshot: ResourceSnapshot<T>,
    action: ResourceSnapshotAction<T>
): ResourceSnapshot<T> {
    switch (action.type) {
        case 'CONNECT':
            return transition(snapshot, {
                data: snapshot.data,
                status: action.reconnect ? 'reconnecting' : 'connecting',
                error: null,
            });
        case 'SNAPSHOT':
            return transition(snapshot, {
                data: action.data,
                status: 'live',
                error: null,
            });
        case 'DISCONNECT':
            return transition(snapshot, {
                data: action.clearData ? null : snapshot.data,
                status: 'reconnecting',
                error: null,
            });
        case 'FAIL':
            return transition(snapshot, {
                data: snapshot.data,
                status: 'error',
                error: action.error,
            });
        case 'CLOSE':
            return transition(snapshot, {
                data: snapshot.data,
                status: 'closed',
                error: null,
            });
    }
}

function transition<T>(
    previous: ResourceSnapshot<T>,
    next: ResourceSnapshot<T>
): ResourceSnapshot<T> {
    if (
        Object.is(previous.data, next.data)
        && previous.status === next.status
        && Object.is(previous.error, next.error)
    ) {
        return previous;
    }

    return Object.freeze(next);
}
