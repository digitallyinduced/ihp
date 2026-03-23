import React, { useState, useEffect, useContext, useSyncExternalStore, useRef, useMemo } from 'react';
import { DataSubscription, DataSyncController } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import type { DataRecord, DynamicSQLQuery, DataSubscriptionOptions, DataSyncEventCallback, ServerMessage } from './types.js';

// Most IHP apps never use this context because they use session cookies for auth.
// Therefore the default value is true.
export const AuthCompletedContext = React.createContext<boolean>(true);

/**
 * Returns the result of the current query in real-time. Returns `null` while the data is still being fetched from the server.
 * @example
 * const messages = useQuery(query('messages').orderBy('createdAt'));
 */
export function useQuery(queryBuilder: QueryBuilder, options: DataSubscriptionOptions | null = null): DataRecord[] | null {
    const dataSubscription = DataSubscriptionStore.get(queryBuilder.query, options);
    const isAuthCompleted = useContext(AuthCompletedContext);
    const records = useSyncExternalStore(dataSubscription.subscribe, dataSubscription.getRecords);

    if (dataSubscription.connectError) {
        throw dataSubscription.connectError;
    }

    if (!isAuthCompleted) {
        return null;
    }

    return records;
}

/**
 * A version of `useQuery` when you only want to fetch a single record.
 *
 * Automatically adds a `.limit(1)` to the query and returns the single result instead of a list.
 *
 * @example
 * const message = useQuerySingleresult(query('messages').filterWhere('id', '1f290b39-c6d1-4dff-8404-0581f470253c'));
 */
export function useQuerySingleResult(queryBuilder: QueryBuilder): DataRecord | null {
    const result = useQuery(queryBuilder.limit(1));
    return result === null ? null : result[0];
}

export function useIsConnected(): boolean {
    const dataSyncController = DataSyncController.getInstance();
    const isConnectedDefault = dataSyncController.connection !== null;

    const [isConnected, setConnected] = useState(isConnectedDefault);

    useEffect(() => {
        const setConnectedTrue: DataSyncEventCallback = () => setConnected(true);
        const setConnectedFalse: DataSyncEventCallback = () => setConnected(false);

        dataSyncController.addEventListener('open', setConnectedTrue);
        dataSyncController.addEventListener('close', setConnectedFalse);

        return () => {
            dataSyncController.removeEventListener('open', setConnectedTrue);
            dataSyncController.removeEventListener('close', setConnectedFalse);
        };
    }, [setConnected]);

    return isConnected;
}

export class DataSubscriptionStore {
    static queryMap: Map<string, DataSubscription> = new Map();

    // To avoid too many loading spinners when going backwards and forwards
    // between pages, we cache the result of queries so we can already showing
    // some data directly after a page transition. The data might be a bit
    // outdated, but it will directly be overriden with the latest server state
    // once it has arrived.
    static cache: Map<string, DataRecord[]> = new Map();

    static get(query: DynamicSQLQuery, options: DataSubscriptionOptions | null = null): DataSubscription {
        const key = JSON.stringify(query) + JSON.stringify(options);
        const existingSubscription = DataSubscriptionStore.queryMap.get(key);

        if (existingSubscription) {
            return existingSubscription;
        } else {

            const subscription = new DataSubscription(query, options, DataSubscriptionStore.cache);
            subscription.createOnServer();
            subscription.onClose = () => { DataSubscriptionStore.queryMap.delete(key); };

            DataSubscriptionStore.queryMap.set(key, subscription);

            // If the query changes very rapid in `useQuery` it can happen that the `dataSubscription.subscribe`
            // is never called at all. In this case we have a unused DataSubscription laying around. We avoid
            // to many open connections laying around by trying to close them a second after opening them.
            // A second is enough time for react to call the subscribe function. If it's not called by then,
            // we most likely deal with a dead subscription, so we close it.
            setTimeout(() => {
                subscription.closeIfNotUsed();
            }, 1000);

            return subscription;
        }
    }
}

export function useCount(queryBuilder: QueryBuilder): number | null {
    const count = useRef<number | null>(null);
    const getSnapshot = useMemo(() => () => count.current, []);
    const subscribe = useMemo(() => (onStoreChange: () => void) => {
        const controller = DataSyncController.getInstance();
        let isActive = true;
        let subscriptionId: string | null = null;
        const onMessage: DataSyncEventCallback = (payload) => {
            const message = payload as ServerMessage;
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
