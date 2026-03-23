// Wire protocol types mirroring Haskell IHP.DataSync.DynamicQuery and IHP.DataSync.Types

export type UUID = string;
export type DataRecord = Record<string, unknown>;

// Condition operators (matches Haskell ConditionOperator)
export type ConditionOperator =
    | 'OpEqual'
    | 'OpGreaterThan'
    | 'OpLessThan'
    | 'OpGreaterThanOrEqual'
    | 'OpLessThanOrEqual'
    | 'OpNotEqual'
    | 'OpAnd'
    | 'OpOr'
    | 'OpIs'
    | 'OpIsNot'
    | 'OpTSMatch'
    | 'OpIn';

// Function calls used in condition expressions
export interface ToTSQueryCall {
    tag: 'ToTSQuery';
    text: string;
}

export type FunctionCall = ToTSQueryCall;

// Condition expressions (matches Haskell ConditionExpression)
export type ConditionExpression =
    | { tag: 'ColumnExpression'; field: string }
    | { tag: 'InfixOperatorExpression'; left: ConditionExpression; op: ConditionOperator; right: ConditionExpression }
    | { tag: 'LiteralExpression'; value: unknown }
    | { tag: 'CallExpression'; functionCall: FunctionCall }
    | { tag: 'ListExpression'; values: unknown[] };

// Selected columns (matches Haskell SelectedColumns)
export type SelectedColumns =
    | { tag: 'SelectAll' }
    | { tag: 'SelectSpecific'; contents: string[] };

// Order by (matches Haskell OrderByClause)
export type OrderByDirection = 'Asc' | 'Desc';

export type OrderByClause =
    | { orderByColumn: string; orderByDirection: OrderByDirection }
    | { tag: 'OrderByTSRank'; tsvector: string; tsquery: string };

// The DynamicSQLQuery structure (matches Haskell DynamicSQLQuery)
export interface DynamicSQLQuery {
    table: string;
    selectedColumns: SelectedColumns;
    whereCondition: ConditionExpression | null;
    orderByClause: OrderByClause[];
    distinctOnColumn: string | null;
    limit: number | null;
    offset: number | null;
}

// DataSubscription options
export const APPEND_NEW_RECORD = 0;
export const PREPEND_NEW_RECORD = 1;

export const NewRecordBehaviour = {
    APPEND_NEW_RECORD,
    PREPEND_NEW_RECORD
} as const;

export interface DataSubscriptionOptions {
    newRecordBehaviour?: number;
}

// CRUD options
export interface CrudOptions {
    transactionId?: UUID | null;
}

// Event system types
export type DataSyncEventType = 'message' | 'close' | 'reconnect' | 'open';
export type DataSyncEventCallback = (payload: unknown) => void;

// Pending request tracking
export interface PendingRequest {
    requestId: number;
    resolve: (value: unknown) => void;
    reject: (reason: unknown) => void;
}

// Server response (loosely typed - the specific shapes vary by tag)
export interface ServerMessage {
    tag: string;
    requestId?: number;
    [key: string]: unknown;
}
