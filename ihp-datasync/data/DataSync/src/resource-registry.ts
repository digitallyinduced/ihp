import { WeakValueMap } from './weak-value-map.js';

/**
 * Lifecycle callbacks attached by a resource factory to the resource it
 * creates. Passing the resource explicitly keeps identity checks reliable and
 * avoids a partially initialized closure while the factory is running.
 */
export type ResourceRegistryLifecycle<Key, Value extends object> = Readonly<{
    key: Key;
    retain(value: Value): void;
    close(value: Value): void;
}>;

export type ResourceRegistryOptions<Input, Key, Value extends object> = Readonly<{
    key(input: Input): Key;
    create(
        input: Input,
        lifecycle: ResourceRegistryLifecycle<Key, Value>,
    ): Value;
}>;

export type ResourceRegistry<Input, Key, Value extends object> = Readonly<{
    /** Strong references to resources with at least one committed owner. */
    readonly active: Map<Key, Value>;
    /**
     * Returns the canonical active or render-pending resource for the input.
     * The lookup itself never promotes the resource into the active map.
     */
    getOrCreate(input: Input): Value;
    /** Replaces the public compatibility Map and forgets render-only values. */
    replaceActive(next: Map<Key, Value>): void;
    /** Clears both active and render-pending resources. */
    clear(): void;
}>;

/**
 * Creates a React- and transport-independent canonical resource registry.
 *
 * Render-only lookups are held weakly. A resource factory can attach the
 * provided lifecycle callbacks and promote the value only when a consumer is
 * committed. Closing is identity-checked so a stale resource cannot evict a
 * newer value for the same key.
 */
export function createResourceRegistry<Input, Key, Value extends object>(
    options: ResourceRegistryOptions<Input, Key, Value>,
): ResourceRegistry<Input, Key, Value> {
    const pending = new WeakValueMap<Key, Value>();
    const instrumentedMaps = new WeakSet<Map<Key, Value>>();
    const resetPending = (): void => pending.clear();
    const instrumentClear = (map: Map<Key, Value>): Map<Key, Value> => {
        if (instrumentedMaps.has(map)) {
            return map;
        }
        const clearMap = map.clear.bind(map);
        try {
            map.clear = (): void => {
                clearMap();
                resetPending();
            };
            instrumentedMaps.add(map);
        } catch (_error) {
            // A frozen compatibility Map is still a valid Map (and its native
            // mutators still work), but its methods cannot be instrumented.
            // Replacing it already reset pending entries; keep accepting the
            // historical writable static-field assignment.
        }
        return map;
    };
    let active = instrumentClear(new Map<Key, Value>());

    const getOrCreate = (input: Input): Value => {
        const key = options.key(input);
        const activeValue = active.get(key);
        if (activeValue !== undefined) {
            return activeValue;
        }

        const pendingValue = pending.get(key);
        if (pendingValue !== undefined) {
            return pendingValue;
        }

        const lifecycle: ResourceRegistryLifecycle<Key, Value> = Object.freeze({
            key,
            retain(value: Value): void {
                active.set(key, value);
            },
            close(value: Value): void {
                if (active.get(key) === value) {
                    active.delete(key);
                }
                pending.delete(key, value);
            },
        });
        const value = options.create(input, lifecycle);
        pending.set(key, value);
        return value;
    };

    const registry: ResourceRegistry<Input, Key, Value> = {
        get active(): Map<Key, Value> {
            return active;
        },
        getOrCreate,
        replaceActive(next: Map<Key, Value>): void {
            active = instrumentClear(next);
            pending.clear();
        },
        clear(): void {
            active.clear();
            pending.clear();
        },
    };
    return Object.freeze(registry);
}
