import { DataSyncController } from './ihp-datasync.js';
import { LiveQueryStore } from './live-query-store.js';
import type {
    LiveQueryStoreOptions,
    QuerySnapshot,
    QuerySnapshotListener,
} from './live-query-store.js';
import type { DataRecord, DataSubscriptionOptions, DynamicSQLQuery } from './types.js';

export type ReactQuerySpec = {
    key: string;
    queryKey: string;
    query: DynamicSQLQuery;
    options: DataSubscriptionOptions | null;
    initialSnapshot: QuerySnapshot;
};

type RegistryEntry = {
    store: LiveQueryStore;
    retainCount: number;
    closeTimeout: ReturnType<typeof setTimeout> | null;
};

type LiveQueryStoreFactory = (options: Omit<LiveQueryStoreOptions, 'controller'>) => LiveQueryStore;

const QUERY_SUBSCRIPTION_DISPOSE_DELAY = 0;

export function createReactQuerySpec(
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null,
    initialRecords: DataRecord[] | null,
): ReactQuerySpec {
    return {
        key: JSON.stringify([query, options]),
        queryKey: JSON.stringify(query),
        query,
        options,
        initialSnapshot: { records: initialRecords, error: null },
    };
}

/** Deduplicates React query consumers and owns their commit-to-cleanup grace period. */
export class ReactQueryRegistry {
    private readonly entries: Map<string, RegistryEntry> = new Map();

    constructor(
        private readonly cache: Map<string, DataRecord[]>,
        private readonly createStore: LiveQueryStoreFactory = options => new LiveQueryStore({
            ...options,
            controller: DataSyncController.getInstance(),
        }),
    ) {}

    getSnapshot(spec: ReactQuerySpec): QuerySnapshot {
        const entry = this.entries.get(spec.key);
        return entry?.store.getSnapshot() ?? spec.initialSnapshot;
    }

    subscribe(spec: ReactQuerySpec, listener: QuerySnapshotListener): () => void {
        let entry = this.entries.get(spec.key);
        let shouldStart = false;

        if (!entry) {
            const store = this.createStore({
                query: spec.query,
                options: spec.options,
                initialRecords: spec.initialSnapshot.records,
                onRecords: records => this.cacheRecords(spec.queryKey, records),
            });
            entry = { store, retainCount: 0, closeTimeout: null };
            this.entries.set(spec.key, entry);
            shouldStart = true;
        }

        const retainedEntry = entry;

        if (retainedEntry.closeTimeout !== null) {
            clearTimeout(retainedEntry.closeTimeout);
            retainedEntry.closeTimeout = null;
        }

        retainedEntry.retainCount++;
        const unsubscribeFromStore = retainedEntry.store.subscribe(listener);
        if (shouldStart) {
            retainedEntry.store.start();
        }

        let isReleased = false;
        return () => {
            if (isReleased) {
                return;
            }
            isReleased = true;
            unsubscribeFromStore();
            retainedEntry.retainCount--;

            if (retainedEntry.retainCount === 0) {
                retainedEntry.closeTimeout = setTimeout(() => {
                    retainedEntry.closeTimeout = null;
                    if (retainedEntry.retainCount > 0) {
                        return;
                    }
                    if (this.entries.get(spec.key) === retainedEntry) {
                        this.entries.delete(spec.key);
                    }
                    retainedEntry.store.dispose();
                }, QUERY_SUBSCRIPTION_DISPOSE_DELAY);
            }
        };
    }

    private cacheRecords(queryKey: string, records: DataRecord[]): void {
        this.cache.set(queryKey, records);
    }
}
