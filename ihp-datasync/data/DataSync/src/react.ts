import React, { useState, useEffect, useContext, useSyncExternalStore, useRef, useMemo } from 'react';
import { DataSubscription, DataSyncController } from './ihp-datasync.js';
import {
    deleteDataSyncRecord,
    detectNewRecordBehaviour,
    insertDataSyncRecord,
    updateDataSyncRecords,
} from './query-subscription.js';
import type { DataSyncQuerySubscription } from './query-subscription.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import type { DataRecord, DynamicSQLQuery, DataSubscriptionOptions, DataSyncEventMap, ServerMessage, UUID } from './types.js';

type QuerySnapshot = {
    records: DataRecord[] | null;
    error: Error | null;
};

type QueryState = QuerySnapshot & { key: string };
type QuerySnapshotListener = (snapshot: QuerySnapshot) => void;
type SharedQuerySubscription = {
    retain(listener: QuerySnapshotListener): () => void;
};

const UNUSED_QUERY_SUBSCRIPTION_CLOSE_DELAY = 0;

/**
 * @deprecated useQuery now uses an effect-owned, ref-counted React store.
 * This class remains available for backwards compatibility with direct users.
 */
export class DataSubscriptionStore {
    static queryMap: Map<string, DataSubscription> = new Map();
    static cache: Map<string, DataRecord[]> = new Map();

    static get(query: DynamicSQLQuery, options: DataSubscriptionOptions | null = null): DataSubscription {
        const key = JSON.stringify(query) + JSON.stringify(options);
        const existingSubscription = DataSubscriptionStore.queryMap.get(key);

        if (existingSubscription) {
            return existingSubscription;
        }

        const subscription = new DataSubscription(query, options, DataSubscriptionStore.cache);
        void subscription.createOnServer();
        subscription.onClose = () => {
            if (DataSubscriptionStore.queryMap.get(key) === subscription) {
                DataSubscriptionStore.queryMap.delete(key);
            }
        };
        DataSubscriptionStore.queryMap.set(key, subscription);
        subscription.scheduleCloseIfNotUsed();
        return subscription;
    }
}

// Keep the last result around when navigating away from and back to a page.
// Reuse the old public cache so existing cache invalidation keeps working.
const queryCache = DataSubscriptionStore.cache;
const activeQuerySubscriptions: Map<string, SharedQuerySubscription> = new Map();

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
    const cachedRecords = queryCache.get(queryKey) ?? null;
    const [queryState, setQueryState] = useState<QueryState>(() => ({
        key: subscriptionKey,
        records: cachedRecords,
        error: null,
    }));
    const currentQueryState = queryState.key === subscriptionKey
        ? queryState
        : { key: subscriptionKey, records: cachedRecords, error: null };

    useEffect(() => acquireQuerySubscription(subscriptionKey, queryKey, query, options, (snapshot) => {
        setQueryState({ key: subscriptionKey, ...snapshot });
    }), [subscriptionKey]);

    if (currentQueryState.error) {
        throw currentQueryState.error;
    }

    if (!isAuthCompleted) {
        return null;
    }

    return currentQueryState.records as TResult[] | null;
}

function acquireQuerySubscription(
    subscriptionKey: string,
    queryKey: string,
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null,
    listener: QuerySnapshotListener,
): () => void {
    let subscription = activeQuerySubscriptions.get(subscriptionKey);
    if (!subscription) {
        subscription = createSharedQuerySubscription(subscriptionKey, queryKey, query, options);
        activeQuerySubscriptions.set(subscriptionKey, subscription);
    }

    return subscription.retain(listener);
}

function createSharedQuerySubscription(
    subscriptionKey: string,
    queryKey: string,
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null,
): SharedQuerySubscription {
    const controller = DataSyncController.getInstance();
    const listeners = new Set<QuerySnapshotListener>();
    const optimisticCreatedPendingRecordIds: UUID[] = [];
    const optimisticUpdatedPendingRecordIds = new Set<UUID>();
    const newRecordBehaviour = options?.newRecordBehaviour ?? detectNewRecordBehaviour(query);
    let records = queryCache.get(queryKey) ?? null;
    let error: Error | null = null;
    let subscriptionId: UUID | null = null;
    let createGeneration = 0;
    let subscriberCount = 0;
    let closeTimeout: ReturnType<typeof setTimeout> | null = null;
    let isActive = true;
    let isRegisteredForOptimisticUpdates = false;

    const getSnapshot = (): QuerySnapshot => ({ records, error });
    const notify = (): void => {
        const snapshot = getSnapshot();
        for (const listener of listeners) {
            listener(snapshot);
        }
    };
    const publish = (newRecords: DataRecord[] | null): void => {
        records = newRecords;
        error = null;
        if (newRecords !== null) {
            queryCache.set(queryKey, newRecords);
        }
        notify();
    };
    const onUpdate = (id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void => {
        if (records === null) {
            optimisticUpdatedPendingRecordIds.delete(id);
            return;
        }

        records = updateDataSyncRecords(
            records,
            id,
            changeSet,
            appendSet,
            !optimisticUpdatedPendingRecordIds.has(id),
        );
        optimisticUpdatedPendingRecordIds.delete(id);
        publish(records);
    };
    const onCreate = (newRecord: DataRecord): void => {
        if (records === null) {
            return;
        }

        const pendingRecordIndex = optimisticCreatedPendingRecordIds.indexOf(newRecord.id);
        if (pendingRecordIndex !== -1) {
            onUpdate(newRecord.id, newRecord, null);
            optimisticCreatedPendingRecordIds.splice(pendingRecordIndex, 1);
            return;
        }

        publish(insertDataSyncRecord(records, newRecord, newRecordBehaviour));
    };
    const optimisticSubscription: DataSyncQuerySubscription = {
        query,
        optimisticUpdatedPendingRecordIds,
        getRecords: () => records,
        onUpdate,
        onCreate,
        onCreateOptimistic: (newRecord: DataRecord): void => {
            onCreate(newRecord);
            optimisticCreatedPendingRecordIds.push(newRecord.id);
        },
        onDelete: (id: UUID): void => {
            if (records !== null) {
                publish(deleteDataSyncRecord(records, id));
            }
        },
    };
    const removeFromOptimisticUpdateRegistry = (): void => {
        if (!isRegisteredForOptimisticUpdates) {
            return;
        }

        controller.removeOptimisticDataSubscription(optimisticSubscription);
        isRegisteredForOptimisticUpdates = false;
    };
    const deleteSubscriptionOnServer = async (id: UUID): Promise<void> => {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: id });
        } catch (deleteError) {
            console.error('useQuery: Failed to delete data subscription', deleteError);
        }
    };
    const createSubscriptionOnServer = async (): Promise<void> => {
        const generation = ++createGeneration;
        try {
            const response = await controller.sendMessage({ tag: 'CreateDataSubscription', query });
            const createdSubscriptionId = response.subscriptionId as UUID;

            if (!isActive || generation !== createGeneration) {
                await deleteSubscriptionOnServer(createdSubscriptionId);
                return;
            }

            subscriptionId = createdSubscriptionId;
            publish(response.result as DataRecord[]);
            controller.learnOptimisticShapeFromResult(query.table, response.result as DataRecord[]);
            if (!isRegisteredForOptimisticUpdates) {
                controller.addOptimisticDataSubscription(optimisticSubscription);
                isRegisteredForOptimisticUpdates = true;
            }
        } catch (connectError) {
            if (!isActive || generation !== createGeneration) {
                return;
            }

            error = new Error((connectError as Error).message + ' while trying to subscribe to:\n' + JSON.stringify(query, null, 4));
            notify();
        }
    };
    const onMessage: DataSyncEventMap['message'] = (message: ServerMessage) => {
        if (message.subscriptionId !== subscriptionId) {
            return;
        }

        if (message.tag === 'DidUpdate') {
            onUpdate(message.id as UUID, message.changeSet as Record<string, unknown> | null, message.appendSet as Record<string, unknown> | null);
        } else if (message.tag === 'DidInsert') {
            onCreate(message.record as DataRecord);
        } else if (message.tag === 'DidDelete') {
            optimisticSubscription.onDelete(message.id as UUID);
        }
    };
    const onClose: DataSyncEventMap['close'] = () => {
        createGeneration++;
        subscriptionId = null;
    };
    const onReconnect: DataSyncEventMap['reconnect'] = () => {
        void createSubscriptionOnServer();
    };
    const dispose = (): void => {
        if (!isActive) {
            return;
        }

        isActive = false;
        createGeneration++;
        controller.removeEventListener('message', onMessage);
        controller.removeEventListener('close', onClose);
        controller.removeEventListener('reconnect', onReconnect);
        removeFromOptimisticUpdateRegistry();
        listeners.clear();

        if (subscriptionId !== null) {
            const activeSubscriptionId = subscriptionId;
            subscriptionId = null;
            void deleteSubscriptionOnServer(activeSubscriptionId);
        }
    };
    const sharedSubscription: SharedQuerySubscription = {
        retain: (listener: QuerySnapshotListener): (() => void) => {
            if (closeTimeout !== null) {
                clearTimeout(closeTimeout);
                closeTimeout = null;
            }
            subscriberCount++;
            listeners.add(listener);
            listener(getSnapshot());

            let isReleased = false;
            return () => {
                if (isReleased) {
                    return;
                }
                isReleased = true;
                listeners.delete(listener);
                subscriberCount--;

                if (subscriberCount === 0) {
                    closeTimeout = setTimeout(() => {
                        closeTimeout = null;
                        if (subscriberCount > 0) {
                            return;
                        }
                        if (activeQuerySubscriptions.get(subscriptionKey) === sharedSubscription) {
                            activeQuerySubscriptions.delete(subscriptionKey);
                        }
                        dispose();
                    }, UNUSED_QUERY_SUBSCRIPTION_CLOSE_DELAY);
                }
            };
        },
    };

    controller.addEventListener('message', onMessage);
    controller.addEventListener('close', onClose);
    controller.addEventListener('reconnect', onReconnect);
    void createSubscriptionOnServer();
    return sharedSubscription;
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
