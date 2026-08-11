import type { SubscriptionProtocol } from './subscription-resource.js';
import type { DynamicSQLQuery, UUID } from './types.js';

/** Wire adapter for the count subscription protocol. */
export function createCountSubscriptionProtocol(
    serverQuery: DynamicSQLQuery,
): SubscriptionProtocol<number> {
    return Object.freeze({
        async create(transport) {
            const response = await transport.request({
                tag: 'CreateCountSubscription',
                query: serverQuery,
            });
            return {
                subscriptionId: response.subscriptionId as UUID,
                value: response.count as number,
                mode: 'legacy',
                revision: 0,
            };
        },
        async delete(transport, subscriptionId) {
            await transport.request({ tag: 'DeleteDataSubscription', subscriptionId });
        },
        isRelevantMessage(message, state) {
            return state.phase.tag === 'live'
                && message.subscriptionId === state.phase.subscriptionId;
        },
        decodeMessage(message, state) {
            if (
                message.tag !== 'DidChangeCount'
                || state.phase.tag !== 'live'
                || message.subscriptionId !== state.phase.subscriptionId
            ) {
                return null;
            }
            return {
                type: 'SERVER_VALUE',
                generation: state.generation,
                subscriptionId: state.phase.subscriptionId,
                value: message.count as number,
            };
        },
    });
}
