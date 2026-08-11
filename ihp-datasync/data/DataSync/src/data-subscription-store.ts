import { DataSubscription, DataSyncController } from './ihp-datasync.js';
import { dataResourceKeyForScope } from './resource-key.js';
import { createResourceRegistry } from './resource-registry.js';
import type { DataRecord, DataSubscriptionOptions, DynamicSQLQuery } from './types.js';

type DataSubscriptionRegistryInput = Readonly<{
    key: string;
    scope: string;
    query: DynamicSQLQuery;
    options: DataSubscriptionOptions | null;
    cache: Map<string, DataRecord[]> | null;
}>;

const dataSubscriptionRegistry = createResourceRegistry<
    DataSubscriptionRegistryInput,
    string,
    DataSubscription
>({
    key: input => input.key,
    create: (input, lifecycle) => {
        const subscription = new DataSubscription(
            input.query,
            input.options,
            input.cache,
            input.key,
            input.scope,
        );
        subscription.onStoreRetain = () => lifecycle.retain(subscription);
        subscription.onStoreClose = () => lifecycle.close(subscription);
        return subscription;
    },
});

/**
 * React-independent registry for shared query resources.
 *
 * Looking up a resource is deliberately inert. The first committed subscriber
 * starts the transport, so abandoned React renders cannot open WebSockets.
 */
export class DataSubscriptionStore {
    static cache = new Map<string, DataRecord[]>();

    // Keep the historical writable Map surface while the functional registry
    // owns canonical pending and active values internally.
    static get queryMap(): Map<string, DataSubscription> {
        return dataSubscriptionRegistry.active;
    }

    static set queryMap(next: Map<string, DataSubscription>) {
        dataSubscriptionRegistry.replaceActive(next);
    }

    static get(
        query: DynamicSQLQuery,
        options: DataSubscriptionOptions | null = null,
    ): DataSubscription {
        const scope = backendScope();
        const key = dataResourceKeyForScope(scope, query, options);
        // A session-cookie user id is intentionally invisible to JavaScript, so
        // it cannot be part of a safe cache key. Cache only JWT-scoped sessions;
        // otherwise a logout/login could briefly expose the previous user's rows.
        const scopedCache = currentJWT() === null ? null : this.cache;
        return dataSubscriptionRegistry.getOrCreate({
            key,
            scope,
            query,
            options,
            cache: scopedCache,
        });
    }
}

export function dataSubscriptionKey(
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null = null,
): string {
    return dataResourceKeyForScope(backendScope(), query, options);
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
