import {
    QueryBuilder,
    ConditionBuilder,
    query,
    ihpBackendUrl,
    fetchAuthenticated,
    filterWhere,
    where,
    whereNot,
    eq,
    notEq,
    or,
    and,
    lessThan,
    lessThanOrEqual,
    greaterThan,
    greaterThanOrEqual,
    whereLessThan,
    whereLessThanOrEqual,
    whereGreaterThan,
    whereGreaterThanOrEqual,
    whereIn
} from './ihp-querybuilder.js';
import { DataSyncController, DataSubscription, initIHPBackend, createRecord, createRecords, updateRecord, updateRecords, deleteRecord, deleteRecords, NewRecordBehaviour } from './ihp-datasync.js';
import { Transaction, withTransaction } from './transaction.js';

export {
    /* ihp-querybuilder.js */
    QueryBuilder,
    ConditionBuilder,
    query,
    ihpBackendUrl,
    fetchAuthenticated,
    filterWhere,
    where,
    whereNot,
    eq,
    notEq,
    or,
    and,
    lessThan,
    lessThanOrEqual,
    greaterThan,
    greaterThanOrEqual,
    whereLessThan,
    whereLessThanOrEqual,
    whereGreaterThan,
    whereGreaterThanOrEqual,
    whereIn,

    /* ihp-datasync.js */
    DataSyncController, DataSubscription, initIHPBackend, createRecord, createRecords, updateRecord, updateRecords, deleteRecord, deleteRecords, NewRecordBehaviour,

    /* transaction.js */
    Transaction, withTransaction
};

/* Re-export types */
export type {
    UUID,
    DataRecord,
    TableName,
    IHPRecord,
    NewRecord,
    TableRegistry,
    NewRecordRegistry,
    DynamicSQLQuery,
    ConditionExpression,
    ConditionOperator,
    SelectedColumns,
    OrderByClause,
    OrderByDirection,
    DataSubscriptionOptions,
    CrudOptions,
    ServerMessage,
} from './types.js';
