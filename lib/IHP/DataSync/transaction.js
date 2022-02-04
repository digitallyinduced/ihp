import { DataSyncController, createRecord, createRecords, updateRecord, updateRecords, deleteRecord, deleteRecords } from "./ihp-datasync.js";
import { query } from "./ihp-querybuilder.js";

export class Transaction {
    constructor() {
        this.transactionId = null;
        this.onClose = this.onClose.bind(this);
        this.dataSyncController = DataSyncController.getInstance();
    }

    async start() {
        const { transactionId } = await this.dataSyncController.sendMessage({ tag: 'StartTransaction' });

        this.transactionId = transactionId;

        this.dataSyncController.addEventListener('close', this.onClose);
    }

    async commit() {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can commit the transaction');
        }
        
        await this.dataSyncController.sendMessage({ tag: 'CommitTransaction', id: this.transactionId });
    }

    async rollback() {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can rollback the transaction');
        }
        
        await this.dataSyncController.sendMessage({ tag: 'RollbackTransaction', id: this.transactionId });
    }

    onClose() {
        this.transactionId = null;
        this.dataSyncController.removeEventListener('close', this.onClose);
    }
    
    getIdOrFail() {
        if (this.transactionId === null) {
            throw new Error('You need to call `.start()` before you can use this transaction');
        }

        return this.transactionId;
    }

    buildOptions() {
        return { transactionId: this.getIdOrFail() };
    }

    query(table) {
        const tableQuery = query(table);
        tableQuery.transactionId = this.getIdOrFail();
        return tableQuery;
    }

    createRecord(table, record) {
        return createRecord(table, record, this.buildOptions());
    }

    createRecords(table, records) {
        return createRecords(table, records, this.buildOptions());
    }

    updateRecord(table, id, patch) {
        return updateRecord(table, id, patch, this.buildOptions());
    }

    updateRecords(table, ids, patch) {
        return updateRecords(table, ids, patch, this.buildOptions());
    }

    deleteRecord(table, id) {
        return deleteRecord(table, id, this.buildOptions());
    }

    deleteRecords(table, ids) {
        return deleteRecords(table, ids, this.buildOptions());
    }
}

export async function withTransaction(callback) {
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
