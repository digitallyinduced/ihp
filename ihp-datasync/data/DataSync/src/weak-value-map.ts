type WeakReference<T extends object> = {
    deref(): T | undefined;
};

type WeakReferenceConstructor = {
    new<T extends object>(target: T): WeakReference<T>;
};

type Finalizer<T> = {
    register(target: object, heldValue: T, unregisterToken?: object): void;
    unregister(unregisterToken: object): boolean;
};

type FinalizerConstructor = {
    new<T>(cleanup: (heldValue: T) => void): Finalizer<T>;
};

type FinalizerEntry<K, V extends object> = {
    key: K;
    reference: WeakReference<V>;
};

const weakReferenceConstructor = (
    globalThis as unknown as { WeakRef?: WeakReferenceConstructor }
).WeakRef;

const finalizerConstructor = (
    globalThis as unknown as { FinalizationRegistry?: FinalizerConstructor }
).FinalizationRegistry;

const maximumStrongFallbackEntries = 1000;

/**
 * A canonical-key registry that does not keep render-only resources alive.
 *
 * Old JavaScript runtimes without WeakRef use a bounded strong-reference
 * fallback. This keeps normal render/commit sharing while preventing abandoned
 * concurrent renders from growing memory without limit. Exceeding the generous
 * bound can only create a duplicate inert resource; it cannot mix snapshots or
 * authentication scopes.
 */
export class WeakValueMap<K, V extends object> {
    private readonly entries = new Map<K, WeakReference<V>>();
    private readonly finalizer: Finalizer<FinalizerEntry<K, V>> | null;

    constructor() {
        this.finalizer = weakReferenceConstructor !== undefined && finalizerConstructor !== undefined
            ? new finalizerConstructor<FinalizerEntry<K, V>>(({ key, reference }) => {
                if (this.entries.get(key) === reference) {
                    this.entries.delete(key);
                }
            })
            : null;
    }

    get(key: K): V | undefined {
        const reference = this.entries.get(key);
        if (reference === undefined) {
            return undefined;
        }
        const value = reference.deref();
        if (value === undefined) {
            this.entries.delete(key);
        }
        return value;
    }

    set(key: K, value: V): void {
        this.delete(key);
        const reference = weakReferenceConstructor !== undefined
            ? new weakReferenceConstructor(value)
            : { deref: () => value };
        this.entries.set(key, reference);
        this.finalizer?.register(value, { key, reference }, value);
        this.pruneCollectedEntries();
        if (weakReferenceConstructor === undefined) {
            this.trimStrongFallback();
        }
    }

    delete(key: K, expectedValue?: V): boolean {
        const reference = this.entries.get(key);
        if (reference === undefined) {
            return false;
        }
        const value = reference.deref();
        if (expectedValue !== undefined && value !== expectedValue) {
            return false;
        }
        this.entries.delete(key);
        if (value !== undefined) {
            this.finalizer?.unregister(value);
        }
        return true;
    }

    clear(): void {
        if (this.finalizer !== null) {
            for (const reference of this.entries.values()) {
                const value = reference.deref();
                if (value !== undefined) {
                    this.finalizer.unregister(value);
                }
            }
        }
        this.entries.clear();
    }

    private pruneCollectedEntries(): void {
        for (const [key, reference] of this.entries) {
            if (reference.deref() === undefined) {
                this.entries.delete(key);
            }
        }
    }

    private trimStrongFallback(): void {
        while (this.entries.size > maximumStrongFallbackEntries) {
            const oldestKey = this.entries.keys().next().value as K | undefined;
            if (oldestKey === undefined) {
                return;
            }
            this.entries.delete(oldestKey);
        }
    }
}
