import React, { useState, useEffect, useContext, useSyncExternalStore, useRef, useMemo } from 'react';
import { DataSyncController } from './ihp-datasync.js';
import type { DataSyncQuerySubscription } from './ihp-datasync.js';
import { QueryBuilder } from './ihp-querybuilder.js';
import { APPEND_NEW_RECORD, PREPEND_NEW_RECORD } from './types.js';
import type { DataRecord, DynamicSQLQuery, DataSubscriptionOptions, DataSyncEventMap, ServerMessage, UUID } from './types.js';

type QueryState = {
    key: string;
    records: DataRecord[] | null;
    error: Error | null;
};

// Keep the last result around when navigating away from and back to a page.
// The server result always replaces this potentially stale value after the
// subscription has connected.
const queryCache: Map<string, DataRecord[]> = new Map();

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

    useEffect(() => {
        const controller = DataSyncController.getInstance();
        const initialRecords = queryCache.get(queryKey) ?? null;
        const optimisticCreatedPendingRecordIds: UUID[] = [];
        const optimisticUpdatedPendingRecordIds = new Set<UUID>();
        const newRecordBehaviour = options?.newRecordBehaviour ?? detectNewRecordBehaviour(query);
        let records = initialRecords;
        let subscriptionId: UUID | null = null;
        let createGeneration = 0;
        let isActive = true;
        let isRegisteredForOptimisticUpdates = false;

        const publish = (newRecords: DataRecord[] | null): void => {
            records = newRecords;
            if (newRecords !== null) {
                queryCache.set(queryKey, newRecords);
            }
            if (isActive) {
                setQueryState({ key: subscriptionKey, records: newRecords, error: null });
            }
        };

        const onUpdate = (id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void => {
            if (records === null) {
                optimisticUpdatedPendingRecordIds.delete(id);
                return;
            }

            const shouldApplyAppendSet = !optimisticUpdatedPendingRecordIds.has(id);
            const updatedRecords = records.map(record => {
                if (record.id !== id) {
                    return record;
                }

                const updated = Object.assign({}, record, changeSet);
                if (appendSet && shouldApplyAppendSet) {
                    for (const [key, value] of Object.entries(appendSet)) {
                        updated[key] = (typeof updated[key] === 'string' ? updated[key] : '') + String(value);
                    }
                }
                return updated;
            });

            optimisticUpdatedPendingRecordIds.delete(id);
            publish(updatedRecords);
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

            const shouldAppend = newRecordBehaviour === APPEND_NEW_RECORD;
            publish(shouldAppend ? [...records, newRecord] : [newRecord, ...records]);
        };

        const subscription: DataSyncQuerySubscription = {
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
                    publish(records.filter(record => record.id !== id));
                }
            },
        };

        const removeFromOptimisticUpdateRegistry = (): void => {
            if (!isRegisteredForOptimisticUpdates) {
                return;
            }

            const index = controller.dataSubscriptions.indexOf(subscription);
            if (index !== -1) {
                controller.dataSubscriptions.splice(index, 1);
            }
            isRegisteredForOptimisticUpdates = false;
        };

        const deleteSubscriptionOnServer = async (id: UUID): Promise<void> => {
            try {
                await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId: id });
            } catch (error) {
                console.error('useQuery: Failed to delete data subscription', error);
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
                    controller.dataSubscriptions.push(subscription);
                    isRegisteredForOptimisticUpdates = true;
                }
            } catch (error) {
                if (!isActive || generation !== createGeneration) {
                    return;
                }

                const connectError = error as Error;
                setQueryState({
                    key: subscriptionKey,
                    records,
                    error: new Error(connectError.message + ' while trying to subscribe to:\n' + JSON.stringify(query, null, 4)),
                });
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
                subscription.onDelete(message.id as UUID);
            }
        };
        const onClose: DataSyncEventMap['close'] = () => {
            createGeneration++;
            subscriptionId = null;
        };
        const onReconnect: DataSyncEventMap['reconnect'] = () => {
            void createSubscriptionOnServer();
        };

        setQueryState({ key: subscriptionKey, records: initialRecords, error: null });
        controller.addEventListener('message', onMessage);
        controller.addEventListener('close', onClose);
        controller.addEventListener('reconnect', onReconnect);
        void createSubscriptionOnServer();

        return () => {
            isActive = false;
            createGeneration++;
            controller.removeEventListener('message', onMessage);
            controller.removeEventListener('close', onClose);
            controller.removeEventListener('reconnect', onReconnect);
            removeFromOptimisticUpdateRegistry();

            if (subscriptionId !== null) {
                const activeSubscriptionId = subscriptionId;
                subscriptionId = null;
                void deleteSubscriptionOnServer(activeSubscriptionId);
            }
        };
    }, [subscriptionKey]);

    if (currentQueryState.error) {
        throw currentQueryState.error;
    }

    if (!isAuthCompleted) {
        return null;
    }

    return currentQueryState.records as TResult[] | null;
}

function detectNewRecordBehaviour(query: DynamicSQLQuery): number {
    const firstOrderBy = query.orderByClause[0];
    const isOrderByCreatedAtDesc = firstOrderBy
        && 'orderByColumn' in firstOrderBy
        && firstOrderBy.orderByColumn === 'createdAt'
        && firstOrderBy.orderByDirection === 'Desc';

    return isOrderByCreatedAtDesc ? PREPEND_NEW_RECORD : APPEND_NEW_RECORD;
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
