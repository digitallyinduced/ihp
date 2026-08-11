import { DataSyncController } from './ihp-datasync.js';
import { ExternalStoreRegistry } from './external-store-registry.js';
import { createLiveCountStore } from './live-count-store.js';
import type { LiveCountStore, LiveCountStoreOptions } from './live-count-store.js';
import type { DynamicSQLQuery } from './types.js';

export type ReactCountSpec = {
    key: string;
    query: DynamicSQLQuery;
};

export function createReactCountSpec(query: DynamicSQLQuery): ReactCountSpec {
    return { key: JSON.stringify(query), query };
}

type LiveCountStoreFactory = (options: Omit<LiveCountStoreOptions, 'controller'>) => LiveCountStore;

export class ReactCountRegistry {
    private readonly registry: ExternalStoreRegistry<ReactCountSpec, number | null>;

    constructor(
        private readonly createStore: LiveCountStoreFactory = options => createLiveCountStore({
            ...options,
            controller: DataSyncController.getInstance(),
        }),
    ) {
        this.registry = new ExternalStoreRegistry(
            spec => this.createStore({ query: spec.query }),
            () => null,
        );
    }

    getSnapshot(spec: ReactCountSpec): number | null {
        return this.registry.getSnapshot(spec);
    }

    subscribe(spec: ReactCountSpec, listener: () => void): () => void {
        return this.registry.subscribe(spec, listener);
    }
}
