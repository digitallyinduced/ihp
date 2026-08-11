import type { DataSyncController } from './ihp-datasync.js';
import type { ManagedExternalStore } from './external-store-registry.js';
import {
    deleteDataSyncRecord,
    detectNewRecordBehaviour,
    insertDataSyncRecord,
    updateDataSyncRecords,
} from './query-subscription.js';
import type { DataSyncQuerySubscription } from './query-subscription.js';
import type {
    DataRecord,
    DataSubscriptionOptions,
    DataSyncEventMap,
    DynamicSQLQuery,
    ServerMessage,
    UUID,
} from './types.js';

export type QuerySnapshot = {
    records: DataRecord[] | null;
    error: Error | null;
};

export type QuerySnapshotListener = () => void;

export type LiveQueryController = Pick<
    DataSyncController,
    | 'sendMessage'
    | 'addEventListener'
    | 'removeEventListener'
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

export type LiveQueryStore = DataSyncQuerySubscription & ManagedExternalStore<QuerySnapshot>;

/** Owns one backend subscription and exposes it as a React-compatible external store. */
export function createLiveQueryStore({
    query,
    options,
    initialRecords,
    controller,
    onRecords,
}: LiveQueryStoreOptions): LiveQueryStore {
    const listeners = new Set<QuerySnapshotListener>();
    const optimisticCreatedPendingRecordIds: UUID[] = [];
    const optimisticUpdatedPendingRecordIds = new Set<UUID>();
    const newRecordBehaviour = options?.newRecordBehaviour ?? detectNewRecordBehaviour(query);
    let snapshot: QuerySnapshot = { records: initialRecords, error: null };
    let subscriptionId: UUID | null = null;
    let createGeneration = 0;
    let isActive = true;
    let isStarted = false;
    let isRegisteredForOptimisticUpdates = false;

    const getSnapshot = (): QuerySnapshot => snapshot;

    const subscribe = (listener: QuerySnapshotListener): (() => void) => {
        listeners.add(listener);
        return () => {
            listeners.delete(listener);
        };
    };

    const getRecords = (): DataRecord[] | null => snapshot.records;

    const notify = (): void => {
        for (const listener of listeners) {
            listener();
        }
    };

    const publishRecords = (records: DataRecord[]): void => {
        snapshot = { records, error: null };
        onRecords(records);
        notify();
    };

    const publishError = (error: Error): void => {
        snapshot = { records: snapshot.records, error };
        notify();
    };

    const onUpdate = (
        id: UUID,
        changeSet: Record<string, unknown> | null,
        appendSet: Record<string, unknown> | null,
    ): void => {
        const records = snapshot.records;
        if (records === null) {
            optimisticUpdatedPendingRecordIds.delete(id);
            return;
        }

        const updatedRecords = updateDataSyncRecords(
            records,
            id,
            changeSet,
            appendSet,
            !optimisticUpdatedPendingRecordIds.has(id),
        );
        optimisticUpdatedPendingRecordIds.delete(id);
        publishRecords(updatedRecords);
    };

    const onCreate = (newRecord: DataRecord): void => {
        const records = snapshot.records;
        if (records === null) {
            return;
        }

        const pendingRecordIndex = optimisticCreatedPendingRecordIds.indexOf(newRecord.id);
        if (pendingRecordIndex !== -1) {
            onUpdate(newRecord.id, newRecord, null);
            optimisticCreatedPendingRecordIds.splice(pendingRecordIndex, 1);
            return;
        }

        publishRecords(insertDataSyncRecord(records, newRecord, newRecordBehaviour));
    };

    const onCreateOptimistic = (newRecord: DataRecord): void => {
        onCreate(newRecord);
        optimisticCreatedPendingRecordIds.push(newRecord.id);
    };

    const onDelete = (id: UUID): void => {
        const records = snapshot.records;
        if (records !== null) {
            publishRecords(deleteDataSyncRecord(records, id));
        }
    };

    const optimisticSubscription: DataSyncQuerySubscription = {
        query,
        optimisticUpdatedPendingRecordIds,
        getRecords,
        onUpdate,
        onCreate,
        onCreateOptimistic,
        onDelete,
    };

    const onMessage: DataSyncEventMap['message'] = (message: ServerMessage) => {
        if (message.subscriptionId !== subscriptionId) {
            return;
        }

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
    };

    const onClose: DataSyncEventMap['close'] = () => {
        createGeneration++;
        subscriptionId = null;
    };

    const deleteSubscriptionOnServer = async (subscriptionId: UUID): Promise<void> => {
        try {
            await controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
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
            const records = response.result as DataRecord[];
            publishRecords(records);
            controller.learnOptimisticShapeFromResult(query.table, records);
            if (!isRegisteredForOptimisticUpdates) {
                controller.addOptimisticDataSubscription(optimisticSubscription);
                isRegisteredForOptimisticUpdates = true;
            }
        } catch (connectError) {
            if (!isActive || generation !== createGeneration) {
                return;
            }

            const error = connectError as Error;
            publishError(new Error(error.message + ' while trying to subscribe to:\n' + JSON.stringify(query, null, 4)));
        }
    };

    const onReconnect: DataSyncEventMap['reconnect'] = () => {
        void createSubscriptionOnServer();
    };

    const removeFromOptimisticUpdateRegistry = (): void => {
        if (!isRegisteredForOptimisticUpdates) {
            return;
        }

        controller.removeOptimisticDataSubscription(optimisticSubscription);
        isRegisteredForOptimisticUpdates = false;
    };

    const start = (): void => {
        if (isStarted || !isActive) {
            return;
        }

        isStarted = true;
        controller.addEventListener('message', onMessage);
        controller.addEventListener('close', onClose);
        controller.addEventListener('reconnect', onReconnect);
        void createSubscriptionOnServer();
    };

    const dispose = (): void => {
        if (!isActive) {
            return;
        }

        isActive = false;
        createGeneration++;
        if (isStarted) {
            controller.removeEventListener('message', onMessage);
            controller.removeEventListener('close', onClose);
            controller.removeEventListener('reconnect', onReconnect);
        }
        removeFromOptimisticUpdateRegistry();
        listeners.clear();

        if (subscriptionId !== null) {
            const activeSubscriptionId = subscriptionId;
            subscriptionId = null;
            void deleteSubscriptionOnServer(activeSubscriptionId);
        }
    };

    return {
        ...optimisticSubscription,
        getSnapshot,
        subscribe,
        start,
        dispose,
    };
}
