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