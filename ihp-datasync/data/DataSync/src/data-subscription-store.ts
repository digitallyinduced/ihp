import { DataSubscription, DataSyncController } from './ihp-datasync.js';
import { WeakValueMap } from './weak-value-map.js';
import type { DataRecord, DataSubscriptionOptions, DynamicSQLQuery } from './types.js';

class ResettableMap<K, V> extends Map<K, V> {
    constructor(private readonly resetPendingEntries: () => void) {
        super();
    }

    override clear(): void {
        super.clear();
        this.resetPendingEntries();
    }
}

/**
 * React-independent registry for shared query resources.
 *
 * Looking up a resource is deliberately inert. The first committed subscriber
 * starts the transport, so abandoned React renders cannot open WebSockets.
 */
export class DataSubscriptionStore {
    private static readonly pendingQueryMap = new WeakValueMap<string, DataSubscription>();
    // Keep the historical public `Map` type so downstream code can replace
    // this test/debug registry without depending on the internal reset hook.
    static queryMap: Map<string, DataSubscription> = new ResettableMap<string, DataSubscription>(() => {
        DataSubscriptionStore.pendingQueryMap.clear();
    });
    static cache = new Map<string, DataRecord[]>();

    static get(
        query: DynamicSQLQuery,
        options: DataSubscriptionOptions | null = null,
    ): DataSubscription {
        const scope = backendScope();
        const key = dataSubscriptionKeyForScope(scope, query, options);
        const existingSubscription = this.queryMap.get(key);
        if (existingSubscription !== undefined) {
            return existingSubscription;
        }
        const pendingSubscription = this.pendingQueryMap.get(key);
        if (pendingSubscription !== undefined) {
            return pendingSubscription;
        }

        // A session-cookie user id is intentionally invisible to JavaScript, so
        // it cannot be part of a safe cache key. Cache only JWT-scoped sessions;
        // otherwise a logout/login could briefly expose the previous user's rows.
        const scopedCache = currentJWT() === null ? null : this.cache;
        const subscription = new DataSubscription(query, options, scopedCache, key, scope);
        subscription.onStoreRetain = () => {
            this.queryMap.set(key, subscription);
        };
        subscription.onStoreClose = () => {
            if (this.queryMap.get(key) === subscription) {
                this.queryMap.delete(key);
            }
            this.pendingQueryMap.delete(key, subscription);
        };
        this.pendingQueryMap.set(key, subscription);
        return subscription;
    }
}

export function dataSubscriptionKey(
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null = null,
): string {
    return dataSubscriptionKeyForScope(backendScope(), query, options);
}

function dataSubscriptionKeyForScope(
    scope: string,
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null,
): string {
    return JSON.stringify([scope, query, options]);
}

function backendScope(): string {
    return DataSyncController.currentTransportScopeKey();
}

function currentJWT(): string | null {
    let jwt: string | null = null;
    try {
        if (typeof localStorage !== 'undefined') {
            jwt = localStorage.getItem('ihp_jwt');
        }
    } catch (_error) {
        // localStorage may be unavailable in SSR or privacy-restricted contexts.
    }
    return jwt;
}
