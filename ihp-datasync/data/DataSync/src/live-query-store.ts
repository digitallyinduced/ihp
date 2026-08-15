import type { DataSyncController } from './ihp-datasync.js';
import type { ManagedExternalStore } from './external-store-registry.js';
import { createLiveSubscriptionStore } from './live-subscription-store.js';
import type { LiveSubscriptionController } from './live-subscription-store.js';
import {
    deleteDataSyncRecord,
    detectNewRecordBehaviour,
    insertDataSyncRecord,
    updateDataSyncRecords,
} from './query-subscription.js';
import type { DataSyncQuerySubscription } from './query-subscription.js';
import type { DataRecord, DataSubscriptionOptions, DynamicSQLQuery, UUID } from './types.js';

export type QuerySnapshot = {
    records: DataRecord[] | null;
    error: Error | null;
};

export type LiveQueryController = LiveSubscriptionController & Pick<
    DataSyncController,
    | 'learnOptimisticShapeFromResult'
    | 'addOptimisticDataSubscription'
    | 'removeOptimisticDataSubscription'
>;

export type LiveQueryStoreOptions = {
    query: DynamicSQLQuery;
    options: DataSubscriptionOptions | null;
    initialRecords: DataRecord[] | null;
    controller: LiveQueryController;
    onRecords: (records: DataRecord[]) => void;
};

/** Owns one backend subscription and exposes it as a React-compatible external store. */
export function createLiveQueryStore({
    query,
    options,
    initialRecords,
    controller,
    onRecords,
}: LiveQueryStoreOptions): ManagedExternalStore<QuerySnapshot> {
    return createLiveSubscriptionStore<QuerySnapshot>(
        controller,
        { records: initialRecords, error: null },
        ({ getSnapshot, publish }) => {
            const optimisticCreatedPendingRecordIds: UUID[] = [];
            const optimisticUpdatedPendingRecordIds = new Set<UUID>();
            const newRecordBehaviour = options?.newRecordBehaviour ?? detectNewRecordBehaviour(query);
            let isRegisteredForOptimisticUpdates = false;

            const publishRecords = (records: DataRecord[]): void => {
                onRecords(records);
                publish({ records, error: null });
            };
            const onUpdate = (
                id: UUID,
                changeSet: Record<string, unknown> | null,
                appendSet: Record<string, unknown> | null,
            ): void => {
                const records = getSnapshot().records;
                if (records === null) {
                    optimisticUpdatedPendingRecordIds.delete(id);
                    return;
                }

                publishRecords(updateDataSyncRecords(
                    records,
                    id,
                    changeSet,
                    appendSet,
                    !optimisticUpdatedPendingRecordIds.has(id),
                ));
                optimisticUpdatedPendingRecordIds.delete(id);
            };
            const onCreate = (record: DataRecord): void => {
                const records = getSnapshot().records;
                if (records === null) {
                    return;
                }

                const pendingIndex = optimisticCreatedPendingRecordIds.indexOf(record.id);
                if (pendingIndex !== -1) {
                    onUpdate(record.id, record, null);
                    optimisticCreatedPendingRecordIds.splice(pendingIndex, 1);
                } else {
                    publishRecords(insertDataSyncRecord(records, record, newRecordBehaviour));
                }
            };
            const onCreateOptimistic = (record: DataRecord): void => {
                onCreate(record);
                optimisticCreatedPendingRecordIds.push(record.id);
            };
            const onDelete = (id: UUID): void => {
                const records = getSnapshot().records;
                if (records !== null) {
                    publishRecords(deleteDataSyncRecord(records, id));
                }
            };
            const optimisticSubscription: DataSyncQuerySubscription = {
                query,
                optimisticUpdatedPendingRecordIds,
                getRecords: () => getSnapshot().records,
                onUpdate,
                onCreate,
                onCreateOptimistic,
                onDelete,
            };

            return {
                createOnServer: () => controller.sendMessage({ tag: 'CreateDataSubscription', query }),
                onCreate: response => {
                    const records = response.result as DataRecord[];
                    publishRecords(records);
                    controller.learnOptimisticShapeFromResult(query.table, records);
                    if (!isRegisteredForOptimisticUpdates) {
                        controller.addOptimisticDataSubscription(optimisticSubscription);
                        isRegisteredForOptimisticUpdates = true;
                    }
                },
                onMessage: message => {
                    if (message.tag === 'DidUpdate') {
                        onUpdate(
                            message.id as UUID,
                            message.changeSet as Record<string, unknown> | null,
                            message.appendSet as Record<string, unknown> | null,
                        );
                    } else if (message.tag === 'DidInsert') {
                        onCreate(message.record as DataRecord);
                    } else if (message.tag === 'DidDelete') {
                        onDelete(message.id as UUID);
                    }
                },
                onCreateError: error => {
                    const connectError = error as Error;
                    publish({
                        records: getSnapshot().records,
                        error: new Error(connectError.message + ' while trying to subscribe to:\n' + JSON.stringify(query, null, 4)),
                    });
                },
                onDeleteError: error => console.error('useQuery: Failed to delete data subscription', error),
                onDispose: () => {
                    if (isRegisteredForOptimisticUpdates) {
                        controller.removeOptimisticDataSubscription(optimisticSubscription);
                        isRegisteredForOptimisticUpdates = false;
                    }
                },
            };
        },
    );
}
