import React, { useState, useEffect } from 'react';
import { DataSubscription } from './ihp-datasync';

// Usage:
// 
//     const messages = useQuery(query('messages').orderBy('createdAt'));
// 
export function useQuery(queryBuilder) {
    const [records, setRecords] = useState(null);

    useEffect(() => {
        const dataSubscription = new DataSubscription(queryBuilder.query);

        dataSubscription.onReady = setRecords;
        dataSubscription.onUpdate = (id, changeSet) => {
            setRecords(records => {
                for (const record of records) {
                    if (record.id === id) {
                        Object.assign(record, changeSet);
                        break;
                    }
                }

                return [...records];
            });
        }
        dataSubscription.onCreate = newRecord => {
            setRecords(records => [...records, newRecord]);
        };
        dataSubscription.onDelete = id => {
            setRecords(records => records.filter(record => record.id !== id));
        };

        return () => { dataSubscription.close() };
    }, [
        JSON.stringify(queryBuilder.query) /* <-- It's terrible - but it works, we should find a better for this */
    ])

    return records;
}