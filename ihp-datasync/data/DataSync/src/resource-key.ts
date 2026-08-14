import type { DataSubscriptionOptions, DynamicSQLQuery } from './types.js';

/** Pure, transport-scoped key shared by row and count resource registries. */
export function dataResourceKeyForScope(
    scope: string,
    query: DynamicSQLQuery,
    options: DataSubscriptionOptions | null = null,
): string {
    return JSON.stringify([scope, query, options]);
}

/** Counts intentionally share the query key format with a distinct namespace. */
export function countResourceKeyForScope(
    scope: string,
    query: DynamicSQLQuery,
): string {
    return `count:${dataResourceKeyForScope(scope, query)}`;
}
