import React, { useCallback, useState, useEffect, useContext, useSyncExternalStore, useMemo } from 'react';
import { DataSyncController } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import { createExternalStoreRegistry } from './external-store-registry.js';
import { DataSubscriptionStore } from './legacy-data-subscription-store.js';
import { createLiveCountStore } from './live-count-store.js';
import { createLiveQueryStore } from './live-query-store.js';
import type { QuerySnapshot } from './live-query-store.js';
import type { DataSubscriptionOptions, DataSyncEventMap, DynamicSQLQuery } from './types.js';

export { DataSubscriptionStore } from './legacy-data-subscription-store.js';

type QuerySpec = {
    key: string;
    queryKey: string;
    query: DynamicSQLQuery;
    options: DataSubscriptionOptions | null;
    initialSnapshot: QuerySnapshot;
};

const reactQueryRegistry = createExternalStoreRegistry<QuerySpec, QuerySnapshot>(
    spec => createLiveQueryStore({
        query: spec.query,
        options: spec.options,
        initialRecords: spec.initialSnapshot.records,
        controller: DataSyncController.getInstance(),
        onRecords: records => DataSubscriptionStore.cache.set(spec.queryKey, records),
    }),
    spec => spec.initialSnapshot,
);

type CountSpec = { key: string; query: DynamicSQLQuery };
const reactCountRegistry = createExternalStoreRegistry<CountSpec, number | null>(
    spec => createLiveCountStore({ query: spec.query, controller: DataSyncController.getInstance() }),
    () => null,
);

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
    const spec = useMemo<QuerySpec>(() => ({
        key: subscriptionKey,
        queryKey,
        query,
        options,
        initialSnapshot: { records: DataSubscriptionStore.cache.get(queryKey) ?? null, error: null },
    }), [subscriptionKey]);
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
    const spec = useMemo<CountSpec>(() => ({ key: queryKey, query }), [queryKey]);
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
