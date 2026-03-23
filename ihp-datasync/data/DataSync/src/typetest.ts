// Type-level test: verify generics narrow with populated registries.
// This file is compiled as part of tsc --noEmit but produces no runtime output.
// All @ts-expect-error directives must suppress real errors or tsc will fail.
import { query, createRecord, updateRecord, deleteRecord } from './index.js';
import type { UUID } from './types.js';

declare module './types.js' {
    interface TableRegistry {
        tasks: { id: UUID; title: string; done: boolean };
        users: { id: UUID; email: string };
    }
    interface NewRecordRegistry {
        tasks: { id?: UUID; title: string; done?: boolean };
        users: { id?: UUID; email: string };
    }
}

// --- Valid code: should compile without errors ---
const q1 = query('tasks');
const q2 = query('tasks').where('title', 'hello');
const q3 = query('tasks').orderBy('title');
async function goodCrud() {
    await createRecord('tasks', { title: 'hello' });
    await createRecord('tasks', { title: 'hello', done: true });
    await updateRecord('tasks', '123' as UUID, { done: true });
    await deleteRecord('tasks', '123' as UUID);
    const tasks = await query('tasks').fetch();
    const t = tasks[0];
    const title: string = t.title;
    const done: boolean = t.done;
}

// --- Invalid code: each line must produce a type error ---

// @ts-expect-error 'nonexistent' is not a valid table name
const bad1 = query('nonexistent');

// @ts-expect-error 'nope' is not a column of tasks
const bad2 = query('tasks').where('nope', 123);

// @ts-expect-error title is string, not number
const bad3 = query('tasks').where('title', 123);

// @ts-expect-error 'nope' is not a column to order by
const bad4 = query('tasks').orderBy('nope');

// @ts-expect-error missing required field 'title'
async function badCreate() { await createRecord('tasks', {}); }

// @ts-expect-error 'nope' does not exist on tasks
async function badUpdate() { await updateRecord('tasks', '123' as UUID, { nope: true }); }
