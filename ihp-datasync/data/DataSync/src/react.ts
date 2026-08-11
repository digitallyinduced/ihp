import React, { useCallback, useContext, useSyncExternalStore } from 'react';
import { DataSyncController } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import { DataSubscriptionStore } from './data-subscription-store.js';
import { CountSubscription, CountSubscriptionStore } from './count-subscription.js';
import { initialResourceSnapshot, type ResourceSnapshot } from './subscription-reducer.js';
import type { DataRecord, DataSubscriptionOptions, DataSyncEventMap } from './types.js';

export { CountSubscription, CountSubscriptionStore } from './count-subscription.js';
export { DataSubscriptionStore } from './data-subscription-store.js';

// Most IHP apps never use this context because they use session cookies for auth.
// Therefore the default value is true.
export const AuthCompletedContext = React.createContext<boolean>(true);

export type AuthCompletedProviderProps = React.PropsWithChildren<{
    value: boolean;
}>;

const disabledQuerySnapshot = initialResourceSnapshot<DataRecord[]>();
const disabledCountSnapshot = initialResourceSnapshot<number>();
const getDisabledQuerySnapshot = (): ResourceSnapshot<DataRecord[]> => disabledQuerySnapshot;
const getDisabledCountSnapshot = (): ResourceSnapshot<number> => disabledCountSnapshot;
const getDisconnectedSnapshot = (): boolean => false;

const subscribeWhileAuthIncomplete = (_listener: () => void): (() => void) => {
    // This runs in useSyncExternalStore's commit-phase subscription. Repeating
    // the reset is intentional: no resource created for the previous opaque
    // cookie-auth session may survive for the next authenticated user.
    DataSyncController.authSessionDidChange();
    return () => {};
};

/**
 * Provides the auth-completion state and invalidates the previous DataSync
 * auth scope whenever auth is incomplete. Unlike using
 * `AuthCompletedContext.Provider` directly, this reset also runs when the
 * provider has no mounted DataSync hooks.
 */
export function AuthCompletedProvider({ value, children }: AuthCompletedProviderProps): React.ReactElement {
    // Deliberately recreated on each render so a committed auth-incomplete
    // tree reasserts the boundary even if imperative code acquired a controller
    // between two commits.
    const subscribeToAuthLifecycle = (_listener: () => void): (() => void) => {
        if (!value) {
            DataSyncController.authSessionDidChange();
        }
        return () => {};
    };
    useSyncExternalStore(
        subscribeToAuthLifecycle,
        getDisconnectedSnapshot,
        getDisconnectedSnapshot,
    );

    return React.createElement(AuthCompletedContext.Provider, { value }, children);
}

/**
 * Returns the exact server result of the current query in real time. It returns
 * `null` until the first server snapshot is available.
 */
export function useQuery<TTable extends string, TResult>(
    queryBuilder: QueryBuilder<TTable, TResult>,
    options: DataSubscriptionOptions | null = null,
): TResult[] | null {
    const isAuthCompleted = useContext(AuthCompletedContext);
    const resource = isAuthCompleted
        ? DataSubscriptionStore.get(queryBuilder.query, options)
        : null;
    const snapshot = useSyncExternalStore(
        resource !== null
            ? resource.subscribeSnapshot
            : listener => subscribeWhileAuthIncomplete(listener),
        resource !== null ? resource.getSnapshot : getDisabledQuerySnapshot,
        getDisabledQuerySnapshot,
    );

    if (snapshot.error !== null) {
        throw snapshot.error;
    }
    return snapshot.data as TResult[] | null;
}

/** Adds `limit(1)` and returns the first result instead of a list. */
export function useQuerySingleResult<TTable extends string, TResult>(
    queryBuilder: QueryBuilder<TTable, TResult>,
): TResult | null {
    const result = useQuery(queryBuilder.limit(1));
    return result === null ? null : result[0] ?? null;
}

export function useCount(queryBuilder: QueryBuilder): number | null {
    const isAuthCompleted = useContext(AuthCompletedContext);
    const resource = isAuthCompleted
        ? CountSubscriptionStore.get(queryBuilder.query)
        : null;
    const snapshot = useSyncExternalStore(
        resource !== null
            ? resource.subscribeSnapshot
            : listener => subscribeWhileAuthIncomplete(listener),
        resource !== null ? resource.getSnapshot : getDisabledCountSnapshot,
        getDisabledCountSnapshot,
    );

    if (snapshot.error !== null) {
        throw snapshot.error;
    }
    return snapshot.data;
}

export function useIsConnected(): boolean {
    const isAuthCompleted = useContext(AuthCompletedContext);
    const transportScopeKey = DataSyncController.currentTransportScopeKey();
    const subscribeToCurrentScope = useCallback(
        (listener: () => void) => connectionResource.subscribe(listener),
        [transportScopeKey],
    );
    return useSyncExternalStore(
        isAuthCompleted
            ? subscribeToCurrentScope
            : listener => subscribeWhileAuthIncomplete(listener),
        isAuthCompleted ? connectionResource.getSnapshot : getDisconnectedSnapshot,
        getDisconnectedSnapshot,
    );
}

type ConnectionExternalStore = Readonly<{
    subscribe(listener: () => void): () => void;
    getSnapshot(): boolean;
    getServerSnapshot(): boolean;
}>;

function createConnectionExternalStore(): ConnectionExternalStore {
    const listeners = new Map<number, () => void>();
    let nextListenerId = 0;
    let controller: DataSyncController | null = null;
    let removeInstanceListener: (() => void) | null = null;

    const notify = (): void => {
        for (const listener of Array.from(listeners.values())) {
            try {
                listener();
            } catch (error) {
                console.error('DataSync connection subscriber failed:', error);
            }
        }
    };

    const onOpen = (_event: Parameters<DataSyncEventMap['open']>[0]): void => {
        notify();
    };

    const onClose = (_event: Parameters<DataSyncEventMap['close']>[0]): void => {
        notify();
    };

    const attachController = (nextController: DataSyncController | null): void => {
        if (controller === nextController) {
            return;
        }
        if (controller !== null) {
            controller.removeEventListener('open', onOpen);
            controller.removeEventListener('close', onClose);
        }
        controller = nextController;
        if (controller !== null) {
            controller.addEventListener('open', onOpen);
            controller.addEventListener('close', onClose);
        }
    };

    const onInstanceChanged = (nextController: DataSyncController | null): void => {
        attachController(nextController);
        notify();
    };

    const subscribe = (listener: () => void): (() => void) => {
        const id = nextListenerId++;
        listeners.set(id, listener);
        if (listeners.size === 1) {
            removeInstanceListener = DataSyncController.addInstanceListener(onInstanceChanged);
            // Subscription happens during commit, so rotating an obsolete auth
            // scope here cannot cause a render-phase side effect.
            attachController(DataSyncController.getInstance());
        }

        let subscribed = true;
        return () => {
            if (!subscribed) {
                return;
            }
            subscribed = false;
            listeners.delete(id);
            if (listeners.size === 0) {
                removeInstanceListener?.();
                removeInstanceListener = null;
                attachController(null);
            }
        };
    };

    const getSnapshot = (): boolean => {
        const controller = DataSyncController.peekInstance();
        return controller !== null && controller.connection !== null;
    };

    const getServerSnapshot = (): boolean => {
        return false;
    };

    return Object.freeze({ subscribe, getSnapshot, getServerSnapshot });
}

const connectionResource = createConnectionExternalStore();
