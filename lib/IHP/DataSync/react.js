import React, { useState, useEffect } from 'react';
import { DataSubscription } from './ihp-datasync.js';

/**
 * Returns the result of the current query in real-time. Returns `null` while the data is still being fetched from the server.
 * @example
 * const messages = useQuery(query('messages').orderBy('createdAt'));
 */
export function useQuery(queryBuilder) {
    const [records, setRecords] = useState(null);

    useEffect(() => {
        const dataSubscription = new DataSubscription(queryBuilder.query);
        dataSubscription.createOnServer();

        // The dataSubscription is automatically closed when the last subscriber on
        // the DataSubscription object has been unsubscribed
        return dataSubscription.subscribe(setRecords);
    }, [
        JSON.stringify(queryBuilder.query) /* <-- It's terrible - but it works, we should find a better for this */
    ])

    return records;
}

/**
 * A version of `useQuery` when you only want to fetch a single record.
 * 
 * Automatically adds a `.limit(1)` to the query and returns the single result instead of a list.
 * 
 * @example
 * const message = useQuerySingleresult(query('messages').filterWhere('id', '1f290b39-c6d1-4dff-8404-0581f470253c'));
 */
export function useQuerySingleResult(queryBuilder) {
    const result = useQuery(queryBuilder.limit(1));
    return result === null ? null : result[0];
}