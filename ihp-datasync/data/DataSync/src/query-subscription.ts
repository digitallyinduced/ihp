import type { DataRecord, DynamicSQLQuery, UUID } from './types.js';
import { APPEND_NEW_RECORD, PREPEND_NEW_RECORD } from './types.js';

/** A live query result that can receive optimistic CRUD updates. */
export interface DataSyncQuerySubscription {
    query: DynamicSQLQuery;
    optimisticUpdatedPendingRecordIds: Set<UUID>;
    getRecords(): DataRecord[] | null;
    onUpdate(id: UUID, changeSet: Record<string, unknown> | null, appendSet: Record<string, unknown> | null): void;
    onCreate(newRecord: DataRecord): void;
    onCreateOptimistic(newRecord: DataRecord): void;
    onDelete(id: UUID): void;
}

export function updateDataSyncRecords(
    records: DataRecord[],
    id: UUID,
    changeSet: Record<string, unknown> | null,
    appendSet: Record<string, unknown> | null,
    shouldApplyAppendSet: boolean,
): DataRecord[] {
    return records.map(record => {
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
}

export function insertDataSyncRecord(records: DataRecord[], newRecord: DataRecord, newRecordBehaviour: number): DataRecord[] {
    return newRecordBehaviour === APPEND_NEW_RECORD
        ? [...records, newRecord]
        : [newRecord, ...records];
}

export function deleteDataSyncRecord(records: DataRecord[], id: UUID): DataRecord[] {
    return records.filter(record => record.id !== id);
}

export function detectNewRecordBehaviour(query: DynamicSQLQuery): number {
    const firstOrderBy = query.orderByClause[0];
    const isOrderByCreatedAtDesc = firstOrderBy
        && 'orderByColumn' in firstOrderBy
        && firstOrderBy.orderByColumn === 'createdAt'
        && firstOrderBy.orderByDirection === 'Desc';

    return isOrderByCreatedAtDesc ? PREPEND_NEW_RECORD : APPEND_NEW_RECORD;
}
