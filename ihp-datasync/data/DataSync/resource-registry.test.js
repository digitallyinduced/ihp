import { createResourceRegistry } from './resource-registry.js';

function createTestRegistry() {
    let creationCount = 0;
    const registry = createResourceRegistry({
        key: input => input.key,
        create: (input, lifecycle) => ({
            creation: ++creationCount,
            input,
            lifecycle,
        }),
    });

    return {
        registry,
        creationCount: () => creationCount,
    };
}

describe('createResourceRegistry', () => {
    test('get-or-create is inert and canonicalizes equal keys while pending', () => {
        const { registry, creationCount } = createTestRegistry();

        const first = registry.getOrCreate({ key: 'tasks', render: 1 });
        const second = registry.getOrCreate({ key: 'tasks', render: 2 });

        expect(second).toBe(first);
        expect(first.input).toEqual({ key: 'tasks', render: 1 });
        expect(creationCount()).toBe(1);
        expect(registry.active.size).toBe(0);
    });

    test('retain promotes a pending resource into the exposed active Map', () => {
        const { registry, creationCount } = createTestRegistry();
        const resource = registry.getOrCreate({ key: 'tasks' });

        resource.lifecycle.retain(resource);

        expect(registry.active).toBeInstanceOf(Map);
        expect(registry.active.get('tasks')).toBe(resource);
        expect(registry.getOrCreate({ key: 'tasks' })).toBe(resource);
        expect(creationCount()).toBe(1);
    });

    test('close removes the current pending and active value', () => {
        const { registry, creationCount } = createTestRegistry();
        const resource = registry.getOrCreate({ key: 'tasks' });
        resource.lifecycle.retain(resource);

        resource.lifecycle.close(resource);
        const replacement = registry.getOrCreate({ key: 'tasks' });

        expect(registry.active.has('tasks')).toBe(false);
        expect(replacement).not.toBe(resource);
        expect(creationCount()).toBe(2);
    });

    test('a stale close cannot evict a newer active value for the same key', () => {
        const { registry, creationCount } = createTestRegistry();
        const stale = registry.getOrCreate({ key: 'tasks', generation: 1 });
        stale.lifecycle.retain(stale);

        registry.clear();
        const current = registry.getOrCreate({ key: 'tasks', generation: 2 });
        current.lifecycle.retain(current);
        stale.lifecycle.close(stale);

        expect(registry.active.get('tasks')).toBe(current);
        expect(registry.getOrCreate({ key: 'tasks' })).toBe(current);
        expect(creationCount()).toBe(2);
    });

    test('clearing the exposed active Map also resets weak pending values', () => {
        const { registry, creationCount } = createTestRegistry();
        const pending = registry.getOrCreate({ key: 'tasks' });
        expect(registry.active.size).toBe(0);

        registry.active.clear();
        const replacement = registry.getOrCreate({ key: 'tasks' });

        expect(replacement).not.toBe(pending);
        expect(creationCount()).toBe(2);
    });

    test('replacing the active compatibility Map resets pending values', () => {
        const { registry, creationCount } = createTestRegistry();
        const pending = registry.getOrCreate({ key: 'tasks' });
        const replacementMap = new Map();

        registry.replaceActive(replacementMap);
        const replacement = registry.getOrCreate({ key: 'tasks' });

        expect(registry.active).toBe(replacementMap);
        expect(replacement).not.toBe(pending);
        expect(creationCount()).toBe(2);
    });

    test('clear still resets pending values after replacing the active Map', () => {
        const { registry, creationCount } = createTestRegistry();
        const replacementMap = new Map();
        registry.replaceActive(replacementMap);
        const pending = registry.getOrCreate({ key: 'tasks' });

        registry.active.clear();
        const replacement = registry.getOrCreate({ key: 'tasks' });

        expect(registry.active).toBe(replacementMap);
        expect(replacement).not.toBe(pending);
        expect(creationCount()).toBe(2);
    });

    test('accepts a frozen compatibility Map without trying to replace its methods', () => {
        const { registry, creationCount } = createTestRegistry();
        const replacementMap = Object.freeze(new Map());

        expect(() => registry.replaceActive(replacementMap)).not.toThrow();
        expect(registry.active).toBe(replacementMap);
        const pending = registry.getOrCreate({ key: 'tasks' });
        expect(pending).toEqual(expect.objectContaining({
            input: { key: 'tasks' },
        }));
        registry.clear();
        expect(registry.getOrCreate({ key: 'tasks' })).not.toBe(pending);
        expect(creationCount()).toBe(2);
    });
});
