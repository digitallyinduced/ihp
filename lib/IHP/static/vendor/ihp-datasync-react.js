// Usage:
// 
//     const messages = useQueryResult(query('messages').orderBy('createdAt'));
// 
function useQueryResult(queryBuilder) {
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
    }, [])

    return records;
}