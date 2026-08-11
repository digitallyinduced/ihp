import type { ManagedExternalStore } from './external-store-registry.js';
import { createLiveSubscriptionStore } from './live-subscription-store.js';
import type { LiveSubscriptionController } from './live-subscription-store.js';
import type { DynamicSQLQuery } from './types.js';

export type LiveCountStoreOptions = {
    query: DynamicSQLQuery;
    controller: LiveSubscriptionController;
};

/** Owns one backend count subscription and keeps it alive across reconnects. */
export function createLiveCountStore({ query, controller }: LiveCountStoreOptions): ManagedExternalStore<number | null> {
    return createLiveSubscriptionStore<number | null>(controller, null, ({ publish }) => ({
        createOnServer: () => controller.sendMessage({ tag: 'CreateCountSubscription', query }),
        onCreate: response => publish(response.count as number),
        onMessage: message => {
            if (message.tag === 'DidChangeCount') {
                publish(message.count as number);
            }
        },
        onCreateError: error => console.error('useCount: Failed to create count subscription', error),
        onDeleteError: error => console.error('useCount: Failed to delete count subscription', error),
    }));
}
