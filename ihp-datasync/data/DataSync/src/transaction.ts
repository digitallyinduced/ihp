import { DataSyncController, createRecord, createRecords, updateRecord, updateRecords, deleteRecord, deleteRecords } from "./ihp-datasync.js";
import { QueryBuilder } from "./ihp-querybuilder.js";
import type { UUID, DataRecord, CrudOptions, DataSyncEventCallback } from "./types.js";

export class Transaction {
    transactionId: UUID | null;
    dataSyncController: DataSyncController;

    constructor() {
        this.transactionId = null;
        this.onClose = this.onClose.bind(this);
        this.dataSyncController = DataSyncController.getInstance();
    }

    async start(): Promise<void> {
        const response = await this.dataSyncController.sendMessage({ tag: 'StartTransaction' });
        this.transactionId = response.transactionId as UUID;

        this.dataSyncController.addEventListener('close', this.onClose as DataSyncEventCallback);
    }

    async commit(): Promise<void> {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can commit the transaction');
        }

        await this.dataSyncController.sendMessage({ tag: 'CommitTransaction', id: this.transactionId });
        this.onClose();
    }

    async rollback(): Promise<void> {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can rollback the transaction');
        }

        await this.dataSyncController.sendMessage({ tag: 'RollbackTransaction', id: this.transactionId });
        this.onClose();
    }

    onClose(): void {
        this.transactionId = null;
        this.dataSyncController.removeEventListener('close', this.onClose as DataSyncEventCallback);
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

    query(table: string): QueryBuilder {
        const tableQuery = new QueryBuilder(table);
        tableQuery.transactionId = this.getIdOrFail();
        return tableQuery;
    }

    createRecord(table: string, record: DataRecord): Promise<DataRecord> {
        return createRecord(table, record, this.buildOptions());
    }

    createRecords(table: string, records: DataRecord[]): Promise<DataRecord[]> {
        return createRecords(table, records, this.buildOptions());
    }

    updateRecord(table: string, id: UUID, patch: DataRecord): Promise<DataRecord> {
        return updateRecord(table, id, patch, this.buildOptions());
    }

    updateRecords(table: string, ids: UUID[], patch: DataRecord): Promise<DataRecord[]> {
        return updateRecords(table, ids, patch, this.buildOptions());
    }

    deleteRecord(table: string, id: UUID): Promise<void> {
        return deleteRecord(table, id, this.buildOptions());
    }

    deleteRecords(table: string, ids: UUID[]): Promise<void> {
        return deleteRecords(table, ids, this.buildOptions());
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
        await transaction.rollback();
        throw exception;
    }
}
