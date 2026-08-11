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

/** Owns one backend subscription and exposes it as a React-compatible external store. */
export class LiveQueryStore implements DataSyncQuerySubscription, ManagedExternalStore<QuerySnapshot> {
    readonly query: DynamicSQLQuery;
    readonly optimisticUpdatedPendingRecordIds: Set<UUID> = new Set();

    private readonly controller: LiveQueryController;
    private readonly listeners: Set<QuerySnapshotListener> = new Set();
    private readonly optimisticCreatedPendingRecordIds: UUID[] = [];
    private readonly newRecordBehaviour: number;
    private readonly onRecords: (records: DataRecord[]) => void;
    private snapshot: QuerySnapshot;
    private subscriptionId: UUID | null = null;
    private createGeneration = 0;
    private isActive = true;
    private isStarted = false;
    private isRegisteredForOptimisticUpdates = false;

    constructor({ query, options, initialRecords, controller, onRecords }: LiveQueryStoreOptions) {
        this.query = query;
        this.controller = controller;
        this.onRecords = onRecords;
        this.newRecordBehaviour = options?.newRecordBehaviour ?? detectNewRecordBehaviour(query);
        this.snapshot = { records: initialRecords, error: null };
    }

    getSnapshot = (): QuerySnapshot => this.snapshot;

    subscribe = (listener: QuerySnapshotListener): (() => void) => {
        this.listeners.add(listener);
        return () => {
            this.listeners.delete(listener);
        };
    };

    start(): void {
        if (this.isStarted || !this.isActive) {
            return;
        }

        this.isStarted = true;
        this.controller.addEventListener('message', this.onMessage);
        this.controller.addEventListener('close', this.onClose);
        this.controller.addEventListener('reconnect', this.onReconnect);
        void this.createSubscriptionOnServer();
    }

    dispose(): void {
        if (!this.isActive) {
            return;
        }

        this.isActive = false;
        this.createGeneration++;
        if (this.isStarted) {
            this.controller.removeEventListener('message', this.onMessage);
            this.controller.removeEventListener('close', this.onClose);
            this.controller.removeEventListener('reconnect', this.onReconnect);
        }
        this.removeFromOptimisticUpdateRegistry();
        this.listeners.clear();

        if (this.subscriptionId !== null) {
            const activeSubscriptionId = this.subscriptionId;
            this.subscriptionId = null;
            void this.deleteSubscriptionOnServer(activeSubscriptionId);
        }
    }

    getRecords(): DataRecord[] | null {
        return this.snapshot.records;
    }

    onUpdate(id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void {
        const records = this.snapshot.records;
        if (records === null) {
            this.optimisticUpdatedPendingRecordIds.delete(id);
            return;
        }

        const updatedRecords = updateDataSyncRecords(
            records,
            id,
            changeSet,
            appendSet,
            !this.optimisticUpdatedPendingRecordIds.has(id),
        );
        this.optimisticUpdatedPendingRecordIds.delete(id);
        this.publishRecords(updatedRecords);
    }

    onCreate(newRecord: DataRecord): void {
        const records = this.snapshot.records;
        if (records === null) {
            return;
        }

        const pendingRecordIndex = this.optimisticCreatedPendingRecordIds.indexOf(newRecord.id);
        if (pendingRecordIndex !== -1) {
            this.onUpdate(newRecord.id, newRecord, null);
            this.optimisticCreatedPendingRecordIds.splice(pendingRecordIndex, 1);
            return;
        }

        this.publishRecords(insertDataSyncRecord(records, newRecord, this.newRecordBehaviour));
    }

    onCreateOptimistic(newRecord: DataRecord): void {
        this.onCreate(newRecord);
        this.optimisticCreatedPendingRecordIds.push(newRecord.id);
    }

    onDelete(id: UUID): void {
        const records = this.snapshot.records;
        if (records !== null) {
            this.publishRecords(deleteDataSyncRecord(records, id));
        }
    }

    private publishRecords(records: DataRecord[]): void {
        this.snapshot = { records, error: null };
        this.onRecords(records);
        this.notify();
    }

    private publishError(error: Error): void {
        this.snapshot = { records: this.snapshot.records, error };
        this.notify();
    }

    private notify(): void {
        for (const listener of this.listeners) {
            listener();
        }
    }

    private readonly onMessage: DataSyncEventMap['message'] = (message: ServerMessage) => {
        if (message.subscriptionId !== this.subscriptionId) {
            return;
        }

        if (message.tag === 'DidUpdate') {
            this.onUpdate(message.id as UUID, message.changeSet as Record<string, unknown> | null, message.appendSet as Record<string, unknown> | null);
        } else if (message.tag === 'DidInsert') {
            this.onCreate(message.record as DataRecord);
        } else if (message.tag === 'DidDelete') {
            this.onDelete(message.id as UUID);
        }
    };

    private readonly onClose: DataSyncEventMap['close'] = () => {
        this.createGeneration++;
        this.subscriptionId = null;
    };

    private readonly onReconnect: DataSyncEventMap['reconnect'] = () => {
        void this.createSubscriptionOnServer();
    };

    private async createSubscriptionOnServer(): Promise<void> {
        const generation = ++this.createGeneration;
        try {
            const response = await this.controller.sendMessage({ tag: 'CreateDataSubscription', query: this.query });
            const createdSubscriptionId = response.subscriptionId as UUID;

            if (!this.isActive || generation !== this.createGeneration) {
                await this.deleteSubscriptionOnServer(createdSubscriptionId);
                return;
            }

            this.subscriptionId = createdSubscriptionId;
            const records = response.result as DataRecord[];
            this.publishRecords(records);
            this.controller.learnOptimisticShapeFromResult(this.query.table, records);
            if (!this.isRegisteredForOptimisticUpdates) {
                this.controller.addOptimisticDataSubscription(this);
                this.isRegisteredForOptimisticUpdates = true;
            }
        } catch (connectError) {
            if (!this.isActive || generation !== this.createGeneration) {
                return;
            }

            const error = connectError as Error;
            this.publishError(new Error(error.message + ' while trying to subscribe to:\n' + JSON.stringify(this.query, null, 4)));
        }
    }

    private removeFromOptimisticUpdateRegistry(): void {
        if (!this.isRegisteredForOptimisticUpdates) {
            return;
        }

        this.controller.removeOptimisticDataSubscription(this);
        this.isRegisteredForOptimisticUpdates = false;
    }

    private async deleteSubscriptionOnServer(subscriptionId: UUID): Promise<void> {
        try {
            await this.controller.sendMessage({ tag: 'DeleteDataSubscription', subscriptionId });
        } catch (deleteError) {
            console.error('useQuery: Failed to delete data subscription', deleteError);
        }
    }
}
