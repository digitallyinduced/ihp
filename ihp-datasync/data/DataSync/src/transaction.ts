import { DataSyncController, createRecord, createRecords, updateRecord, updateRecords, deleteRecord, deleteRecords } from "./ihp-datasync.js";
import { QueryBuilder } from "./ihp-querybuilder.js";
import type { UUID, CrudOptions, DataSyncEventMap, TableName, IHPRecord, NewRecord } from "./types.js";

export class Transaction {
    transactionId: UUID | null;
    dataSyncController: DataSyncController;
    private readonly transactionController: DataSyncController;

    constructor() {
        this.transactionId = null;
        this.onClose = this.onClose.bind(this);
        this.transactionController = DataSyncController.getInstance();
        // Keep the historically writable public field as a compatibility
        // facade. Internal operations remain pinned to the captured controller
        // even if application code reassigns this property.
        this.dataSyncController = this.transactionController;
    }

    async start(): Promise<void> {
        const response = await this.transactionController.sendMessage({ tag: 'StartTransaction' });
        this.transactionId = response.transactionId as UUID;

        this.transactionController.addEventListener('close', this.onClose);
    }

    async commit(): Promise<void> {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can commit the transaction');
        }

        await this.transactionController.sendMessage({ tag: 'CommitTransaction', id: this.transactionId });
        this.onClose();
    }

    async rollback(): Promise<void> {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can rollback the transaction');
        }

        await this.transactionController.sendMessage({ tag: 'RollbackTransaction', id: this.transactionId });
        this.onClose();
    }

    onClose(): void {
        this.transactionId = null;
        this.transactionController.removeEventListener('close', this.onClose);
    }

    getIdOrFail(): UUID {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can use this transaction');
        }

        return this.transactionId;
    }

    buildOptions(): CrudOptions {
        return { transactionId: this.getIdOrFail() };
    }

    query<T extends TableName>(table: T): QueryBuilder<T, IHPRecord<T>> {
        const tableQuery = new QueryBuilder<T, IHPRecord<T>>(table, undefined, this.transactionController);
        tableQuery.transactionId = this.getIdOrFail();
        return tableQuery;
    }

    createRecord<T extends TableName>(table: T, record: NewRecord<T>): Promise<IHPRecord<T>> {
        return createRecord(table, record, this.buildOptions(), this.transactionController);
    }

    createRecords<T extends TableName>(table: T, records: NewRecord<T>[]): Promise<IHPRecord<T>[]> {
        return createRecords(table, records, this.buildOptions(), this.transactionController);
    }

    updateRecord<T extends TableName>(table: T, id: UUID, patch: Partial<NewRecord<T>>): Promise<IHPRecord<T>> {
        return updateRecord(table, id, patch, this.buildOptions(), this.transactionController);
    }

    updateRecords<T extends TableName>(table: T, ids: UUID[], patch: Partial<NewRecord<T>>): Promise<IHPRecord<T>[]> {
        return updateRecords(table, ids, patch, this.buildOptions(), this.transactionController);
    }

    deleteRecord<T extends TableName>(table: T, id: UUID): Promise<void> {
        return deleteRecord(table, id, this.buildOptions(), this.transactionController);
    }

    deleteRecords<T extends TableName>(table: T, ids: UUID[]): Promise<void> {
        return deleteRecords(table, ids, this.buildOptions(), this.transactionController);
    }
}

export async function withTransaction<T>(callback: (transaction: Transaction) => Promise<T>): Promise<T> {
    const transaction = new Transaction();
    await transaction.start();
    try {
        const result = await callback(transaction);
        await transaction.commit();
        return result;
    } catch (exception) {
        if (transaction.transactionId !== null) {
            try {
                await transaction.rollback();
            } catch (rollbackError) {
                // The callback/commit failure is the primary error. In
                // particular, auth-scope retirement clears or invalidates the
                // transaction and a second rollback failure must not mask it.
                console.error('Failed to roll back a DataSync transaction:', rollbackError);
            }
        }
        throw exception;
    }
}
