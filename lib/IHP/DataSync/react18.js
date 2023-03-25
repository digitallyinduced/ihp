import React, { useState, useEffect, useSyncExternalStore } from 'react';
import { DataSubscription } from './ihp-datasync.js';

/**
 * Returns the result of the current query in real-time. Suspends while the data is still being fetched from the server.
 * @example
 * const messages = useQuery(query('messages').orderBy('createdAt'));
 */
export function useQuery(queryBuilder) {
    const dataSubscription = DataSubscriptionStore.get(queryBuilder.query);

    if (dataSubscription.isConnected) {
        const records = useSyncExternalStore(dataSubscription.subscribe, dataSubscription.getRecords)
        return records;
    } else if (dataSubscription.connectError) {
        throw dataSubscription.connectError;
    } else {
        throw dataSubscription.createOnServerPromise;
    }
}

export class DataSubscriptionStore {
    static queryMap = new Map();
    static get(query) {
        const strinigifiedQuery = JSON.stringify(query);
        const existingSubscription = DataSubscriptionStore.queryMap.get(strinigifiedQuery)

        if (existingSubscription) {
            return existingSubscription;
        } else {
            const subscription = new DataSubscription(query);
            subscription.createOnServer();
            subscription.onClose = () => { DataSubscriptionStore.queryMap.delete(strinigifiedQuery); };

            DataSubscriptionStore.queryMap.set(strinigifiedQuery, subscription);

            // If the query changes very rapid in `useQuery` it can happen that the `dataSubscription.subscribe`
            // is never called at all. In this case we have a unused DataSubscription laying around. We avoid
            // to many open connections laying around by trying to close them a second after opening them.
            // A second is enough time for react to call the subscribe function. If it's not called by then,
            // we most likely deal with a dead subscription, so we close it.
            setTimeout(() => {
                subscription.closeIfNotUsed();
            }, 1000);

            return subscription;
        }
    }
}