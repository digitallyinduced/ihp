import { DataSubscription } from './ihp-datasync.js';
import type { DataRecord, DataSubscriptionOptions, DynamicSQLQuery } from './types.js';

/**
 * @deprecated useQuery uses an effect-owned React query registry.
 * This class remains available for backwards compatibility with direct users.
 */
export class DataSubscriptionStore {
    static queryMap: Map<string, DataSubscription> = new Map();
    static cache: Map<string, DataRecord[]> = new Map();

    static get(query: DynamicSQLQuery, options: DataSubscriptionOptions | null = null): DataSubscription {
        const key = JSON.stringify(query) + JSON.stringify(options);
        const existingSubscription = DataSubscriptionStore.queryMap.get(key);

        if (existingSubscription) {
            return existingSubscription;
        }

        const subscription = new DataSubscription(query, options, DataSubscriptionStore.cache);
        void subscription.createOnServer();
        subscription.onClose = () => {
            if (DataSubscriptionStore.queryMap.get(key) === subscription) {
                DataSubscriptionStore.queryMap.delete(key);
            }
        };
        DataSubscriptionStore.queryMap.set(key, subscription);
        subscription.scheduleCloseIfNotUsed();
        return subscription;
    }
}
