import type { SubscriptionEvent, SubscriptionState } from './subscription-machine.js';
import type { SubscriptionProtocol } from './subscription-resource.js';
import type { DataRecord, DynamicSQLQuery, ServerMessage, UUID } from './types.js';

/** Wire adapter for revisioned row snapshots and legacy invalidations. */
export function createDataSubscriptionProtocol(
    serverQuery: DynamicSQLQuery,
): SubscriptionProtocol<DataRecord[]> {
    return Object.freeze({
        async create(transport) {
            const response = await transport.request({
                tag: 'CreateDataSubscription',
                query: serverQuery,
                protocolVersion: 1,
            });
            const snapshotMode = response.tag === 'DidCreateDataSubscriptionV2';
            return {
                subscriptionId: response.subscriptionId as UUID,
                value: cloneDataRecords(response.result as DataRecord[]),
                mode: snapshotMode ? 'snapshot' : 'legacy',
                revision: snapshotMode && typeof response.revision === 'number'
                    ? response.revision
                    : 0,
            };
        },
        async delete(transport, subscriptionId) {
            await transport.request({ tag: 'DeleteDataSubscription', subscriptionId });
        },
        async refresh(transport) {
            const response = await transport.request({
                tag: 'DataSyncQuery',
                query: serverQuery,
                transactionId: null,
            });
            return cloneDataRecords(response.result as DataRecord[]);
        },
        isRelevantMessage(message, state) {
            return state.phase.tag === 'live'
                && message.subscriptionId === state.phase.subscriptionId;
        },
        decodeMessage: decodeDataSubscriptionMessage,
        createError(unknownError) {
            const error = unknownError instanceof Error
                ? unknownError
                : new Error(String(unknownError));
            return new Error(
                error.message
                + ' while trying to subscribe to:\n'
                + JSON.stringify(serverQuery, null, 4),
            );
        },
    });
}

export function decodeDataSubscriptionMessage(
    message: ServerMessage,
    state: SubscriptionState<DataRecord[]>,
): SubscriptionEvent<DataRecord[]> | null {
    if (
        state.phase.tag !== 'live'
        || message.subscriptionId !== state.phase.subscriptionId
    ) {
        return null;
    }
    if (message.tag === 'DidReplaceDataSubscription') {
        return {
            type: 'SERVER_SNAPSHOT',
            generation: state.generation,
            subscriptionId: state.phase.subscriptionId,
            value: cloneDataRecords(message.result as DataRecord[]),
            revision: typeof message.revision === 'number'
                ? message.revision
                : state.phase.revision + 1,
        };
    }
    if (
        state.phase.mode === 'legacy'
        && (message.tag === 'DidInsert'
            || message.tag === 'DidUpdate'
            || message.tag === 'DidDelete')
    ) {
        return {
            type: 'LEGACY_INVALIDATED',
            generation: state.generation,
            subscriptionId: state.phase.subscriptionId,
        };
    }
    return null;
}

export function cloneDataRecords(records: DataRecord[]): DataRecord[] {
    return records.map(record => ({ ...record })) as DataRecord[];
}
