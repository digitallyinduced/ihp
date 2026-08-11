export interface ManagedExternalStore<TSnapshot> {
    getSnapshot(): TSnapshot;
    subscribe(listener: () => void): () => void;
    start(): void;
    dispose(): void;
}

export type ExternalStoreSpec = {
    key: string;
};

type RegistryEntry<TSnapshot> = {
    store: ManagedExternalStore<TSnapshot>;
    retainCount: number;
    closeTimeout: ReturnType<typeof setTimeout> | null;
};

const STORE_DISPOSE_DELAY = 0;

/** Owns commit-time acquisition, deduplication, refcounts and Strict Mode cleanup grace. */
export class ExternalStoreRegistry<TSpec extends ExternalStoreSpec, TSnapshot> {
    private readonly entries: Map<string, RegistryEntry<TSnapshot>> = new Map();

    constructor(
        private readonly createStore: (spec: TSpec) => ManagedExternalStore<TSnapshot>,
        private readonly getInitialSnapshot: (spec: TSpec) => TSnapshot,
    ) {}

    getSnapshot(spec: TSpec): TSnapshot {
        const entry = this.entries.get(spec.key);
        return entry?.store.getSnapshot() ?? this.getInitialSnapshot(spec);
    }

    subscribe(spec: TSpec, listener: () => void): () => void {
        let entry = this.entries.get(spec.key);
        let shouldStart = false;

        if (!entry) {
            entry = {
                store: this.createStore(spec),
                retainCount: 0,
                closeTimeout: null,
            };
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
                }, STORE_DISPOSE_DELAY);
            }
        };
    }
}
