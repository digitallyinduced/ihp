import React, { useState, useEffect, useContext } from 'react';
import { DataSubscription, DataSyncController } from './ihp-datasync.js';

// Most IHP apps never use this context because they use session cookies for auth.
// Therefore the default value is true.
export const AuthCompletedContext = React.createContext(true);

// To avoid too many loading spinners when going backwards and forwards
// between pages, we cache the result of queries so we can already showing
// some data directly after a page transition. The data might be a bit
// outdated, but it will directly be overriden with the latest server state
// once it has arrived.
const recordsCache = new Map();

/**
 * Returns the result of the current query in real-time. Returns `null` while the data is still being fetched from the server.
 * @example
 * const messages = useQuery(query('messages').orderBy('createdAt'));
 */
export function useQuery(queryBuilder) {
    const [records, setRecords] = useState(() => {
        const strinigifiedQuery = JSON.stringify(queryBuilder.query);
        const cachedRecords = recordsCache.get(strinigifiedQuery);
        return cachedRecords === undefined ? null : cachedRecords;
    });
    const isAuthCompleted = useContext(AuthCompletedContext);

    useEffect(() => {
        if (!isAuthCompleted) {
            return;
        }

        const strinigifiedQuery = JSON.stringify(queryBuilder.query);
        const cachedRecords = recordsCache.get(strinigifiedQuery);
        
        // Invalidate existing records, as the query might have been changed
        setRecords(cachedRecords === undefined ? null : cachedRecords);

        const dataSubscription = new DataSubscription(queryBuilder.query);
        dataSubscription.createOnServer();

        // The dataSubscription is automatically closed when the last subscriber on
        // the DataSubscription object has been unsubscribed

        return dataSubscription.subscribe(records => {
            setRecords(records);

            // Update the cache whenever the records change
            recordsCache.set(strinigifiedQuery, records);
        });
    }, [
        JSON.stringify(queryBuilder.query) /* <-- It's terrible - but it works, we should find a better for this */,
        isAuthCompleted
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

export function useIsConnected() {
    const dataSyncController = DataSyncController.getInstance();
    const isConnectedDefault = dataSyncController.connection !== null;
    
    const [isConnected, setConnected] = useState(isConnectedDefault);

    useEffect(() => {
        const setConnectedTrue = () => setConnected(true);
        const setConnectedFalse = () => setConnected(false);
        
        dataSyncController.addEventListener('open', setConnectedTrue);
        dataSyncController.addEventListener('close', setConnectedFalse);

        return () => {
            dataSyncController.removeEventListener('open', setConnectedTrue);
            dataSyncController.removeEventListener('close', setConnectedFalse);
        }
    }, [ setConnected ]);

    return isConnected;
}