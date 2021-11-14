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
            for (const record of records) {
                if (record.id === id) {
                    Object.assign(record, changeSet);
                    break;
                }
            }

            setRecords(records);
        }
        dataSubscription.onCreate = newRecord => {
            records.push(newRecord);
            setRecords(records);
        };
        dataSubscription.onDelete = id => {
            setRecords(records.filter(record => record.id !== id));
        };

        return () => { dataSubscription.close() };
    })

    return records;
}
