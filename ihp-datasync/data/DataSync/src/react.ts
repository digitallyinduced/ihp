import React, { useCallback, useState, useEffect, useContext, useSyncExternalStore, useMemo } from 'react';
import { DataSyncController } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import { DataSubscriptionStore } from './legacy-data-subscription-store.js';
import { createReactCountSpec, ReactCountRegistry } from './react-count-registry.js';
import { createReactQuerySpec, ReactQueryRegistry } from './react-query-registry.js';
import type { DataSubscriptionOptions, DataSyncEventMap } from './types.js';

export { DataSubscriptionStore } from './legacy-data-subscription-store.js';

const reactQueryRegistry = new ReactQueryRegistry(DataSubscriptionStore.cache);
const reactCountRegistry = new ReactCountRegistry();

// Most IHP apps never use this context because they use session cookies for auth.
// Therefore the default value is true.
export const AuthCompletedContext = React.createContext<boolean>(true);

/**
 * Returns the result of the current query in real-time. Returns `null` while the data is still being fetched from the server.
 * @example
 * const messages = useQuery(query('messages').orderBy('createdAt'));
 */
export function useQuery<TTable extends string, TResult>(queryBuilder: QueryBuilder<TTable, TResult>, options: DataSubscriptionOptions | null = null): TResult[] | null {
    const isAuthCompleted = useContext(AuthCompletedContext);
    const query = queryBuilder.query;
    const queryKey = JSON.stringify(query);
    const subscriptionKey = JSON.stringify([query, options]);
    const spec = useMemo(
        () => createReactQuerySpec(query, options, DataSubscriptionStore.cache.get(queryKey) ?? null),
        [subscriptionKey],
    );
    const subscribe = useCallback(
        (listener: () => void) => isAuthCompleted
            ? reactQueryRegistry.subscribe(spec, listener)
            : () => {},
        [spec, isAuthCompleted],
    );
    const getSnapshot = useCallback(() => reactQueryRegistry.getSnapshot(spec), [spec]);
    const snapshot = useSyncExternalStore(subscribe, getSnapshot);

    if (!isAuthCompleted) {
        return null;
    }

    if (snapshot.error) {
        throw snapshot.error;
    }

    return snapshot.records as TResult[] | null;
}

/**
 * A version of `useQuery` when you only want to fetch a single record.
 *
 * Automatically adds a `.limit(1)` to the query and returns the single result instead of a list.
 *
 * @example
 * const message = useQuerySingleresult(query('messages').filterWhere('id', '1f290b39-c6d1-4dff-8404-0581f470253c'));
 */
export function useQuerySingleResult<TTable extends string, TResult>(queryBuilder: QueryBuilder<TTable, TResult>): TResult | null {
    const result = useQuery(queryBuilder.limit(1));
    return result === null ? null : result[0];
}

export function useIsConnected(): boolean {
    const dataSyncController = DataSyncController.getInstance();
    const isConnectedDefault = dataSyncController.connection !== null;

    const [isConnected, setConnected] = useState(isConnectedDefault);

    useEffect(() => {
        const setConnectedTrue: DataSyncEventMap['open'] = () => setConnected(true);
        const setConnectedFalse: DataSyncEventMap['close'] = () => setConnected(false);

        dataSyncController.addEventListener('open', setConnectedTrue);
        dataSyncController.addEventListener('close', setConnectedFalse);

        return () => {
            dataSyncController.removeEventListener('open', setConnectedTrue);
            dataSyncController.removeEventListener('close', setConnectedFalse);
        };
    }, [setConnected]);

    return isConnected;
}

export function useCount(queryBuilder: QueryBuilder): number | null {
    const isAuthCompleted = useContext(AuthCompletedContext);
    const query = queryBuilder.query;
    const queryKey = JSON.stringify(query);
    const spec = useMemo(() => createReactCountSpec(query), [queryKey]);
    const subscribe = useCallback(
        (listener: () => void) => isAuthCompleted
            ? reactCountRegistry.subscribe(spec, listener)
            : () => {},
        [spec, isAuthCompleted],
    );
    const getSnapshot = useCallback(() => reactCountRegistry.getSnapshot(spec), [spec]);
    const count = useSyncExternalStore(subscribe, getSnapshot);

    return isAuthCompleted ? count : null;
}
