import React, { useCallback, useState, useEffect, useContext, useSyncExternalStore, useRef, useMemo } from 'react';
import { DataSyncController } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import { DataSubscriptionStore } from './legacy-data-subscription-store.js';
import { createReactQuerySpec, ReactQueryRegistry } from './react-query-registry.js';
import type { DataSubscriptionOptions, DataSyncEventMap } from './types.js';

export { DataSubscriptionStore } from './legacy-data-subscription-store.js';

const reactQueryRegistry = new ReactQueryRegistry(DataSubscriptionStore.cache);

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
        (listener: () => void) => reactQueryRegistry.subscribe(spec, listener),
        [spec],
    );
    const getSnapshot = useCallback(() => reactQueryRegistry.getSnapshot(spec), [spec]);
    const snapshot = useSyncExternalStore(subscribe, getSnapshot);

    if (snapshot.error) {
        throw snapshot.error;
    }

    if (!isAuthCompleted) {
        return null;
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
    const count = useRef<number | null>(null);
    const getSnapshot = useMemo(() => () => count.current, []);
    const subscribe = useMemo(() => (onStoreChange: () => void) => {
        const controller = DataSyncController.getInstance();
        let isActive = true;
        let subscriptionId: string | null = null;
        const onMessage: DataSyncEventMap['message'] = (message) => {
            if (message.tag === 'DidChangeCount' && message.subscriptionId === subscriptionId) {
                count.current = message.count as number;
                onStoreChange();
            }
        };
        controller.sendMessage({ tag: 'CreateCountSubscription', query: queryBuilder.query })
            .then((response) => {
                if (isActive) {
                    subscriptionId = response.subscriptionId as string;
                    count.current = response.count as number;
                    onStoreChange();

                    controller.addEventListener('message', onMessage);
                } else {
                    controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: response.subscriptionId });
                }
            })
            .catch((error: unknown) => {
                console.error('useCount: Failed to create count subscription', error);
            });

        return () => {
            isActive = false;

            if (subscriptionId) {
                controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
            }
            controller.removeEventListener('message', onMessage);
        };
    }, [JSON.stringify(queryBuilder.query)]);

    return useSyncExternalStore(subscribe, getSnapshot);
}
