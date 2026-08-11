import { DataSyncController } from './ihp-datasync.js';
import { ExternalStoreRegistry } from './external-store-registry.js';
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

type LiveQueryStoreFactory = (options: Omit<LiveQueryStoreOptions, 'controller'>) => LiveQueryStore;

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
    private readonly registry: ExternalStoreRegistry<ReactQuerySpec, QuerySnapshot>;

    constructor(
        private readonly cache: Map<string, DataRecord[]>,
        private readonly createStore: LiveQueryStoreFactory = options => new LiveQueryStore({
            ...options,
            controller: DataSyncController.getInstance(),
        }),
    ) {
        this.registry = new ExternalStoreRegistry(
            spec => this.createStore({
                query: spec.query,
                options: spec.options,
                initialRecords: spec.initialSnapshot.records,
                onRecords: records => this.cacheRecords(spec.queryKey, records),
            }),
            spec => spec.initialSnapshot,
        );
    }

    getSnapshot(spec: ReactQuerySpec): QuerySnapshot {
        return this.registry.getSnapshot(spec);
    }

    subscribe(spec: ReactQuerySpec, listener: QuerySnapshotListener): () => void {
        return this.registry.subscribe(spec, listener);
    }

    private cacheRecords(queryKey: string, records: DataRecord[]): void {
        this.cache.set(queryKey, records);
    }
}
