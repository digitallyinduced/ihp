export interface ManagedExternalStore<TSnapshot> {
    getSnapshot(): TSnapshot;
    subscribe(listener: () => void): () => void;
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
export function createExternalStoreRegistry<TSpec extends ExternalStoreSpec, TSnapshot>(
    createStore: (spec: TSpec) => ManagedExternalStore<TSnapshot>,
    getInitialSnapshot: (spec: TSpec) => TSnapshot,
) {
    const entries = new Map<string, RegistryEntry<TSnapshot>>();

    const getSnapshot = (spec: TSpec): TSnapshot => {
        const entry = entries.get(spec.key);
        return entry ? entry.store.getSnapshot() : getInitialSnapshot(spec);
    };

    const subscribe = (spec: TSpec, listener: () => void): (() => void) => {
        let entry = entries.get(spec.key);

        if (!entry) {
            entry = {
                store: createStore(spec),
                retainCount: 0,
                closeTimeout: null,
            };
            entries.set(spec.key, entry);
        }

        const retainedEntry = entry;
        if (retainedEntry.closeTimeout !== null) {
            clearTimeout(retainedEntry.closeTimeout);
            retainedEntry.closeTimeout = null;
        }

        retainedEntry.retainCount++;
        const unsubscribeFromStore = retainedEntry.store.subscribe(listener);

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
                    if (entries.get(spec.key) === retainedEntry) {
                        entries.delete(spec.key);
                    }
                    retainedEntry.store.dispose();
                }, STORE_DISPOSE_DELAY);
            }
        };
    };

    return { getSnapshot, subscribe };
}
