function fetchAuthenticated(path, params) {
    const jwt = localStorage.getItem('ihp_jwt');
    if (jwt !== null) {
        params.headers['Authorization'] = 'Bearer ' + jwt;
    } else {
        params.credentials = 'include';
    }

    return fetch(ihpBackendUrl(path), params);
}

function ihpBackendUrl(path) {
    const host = window.ihpBackendHost || '';
    return host + path;
}

window.fetchAuthenticated = fetchAuthenticated;

class QueryBuilder {
    constructor(table) {
        // Maps to 'DynamicSQLQuery'
        this.query = {
            table,
            selectedColumns: { tag: 'SelectAll' },
            whereCondition: null,
            orderByClause: [],
            limitClause: null,
            offsetClause: null
        };
    }

    filterWhere(field, value) {
        this.query.whereCondition = { tag: 'VarCondition', contents: [field + ' = ?', value ] };

        return this;
    }

    orderBy(column) {
        return this.orderByAsc(column);
    }

    orderByAsc(column) {
        this.query.orderByClause.push({ orderByColumn: column, orderByDirection: 'Asc' });

        return this;
    }

    orderByDesc(column) {
        this.query.orderByClause.push({ orderByColumn: column, orderByDirection: 'Desc' });

        return this;
    }

    limit(limit) {
        this.query.limitClause = 'LIMIT ' + limit;

        return this;
    }

    offset(offset) {
        this.query.offsetClause = 'OFFSET ' + offset;

        return this;
    }

    async fetch() {
        // return fetch('/Query').then(response => response.json());
        const { result } = await DataSyncController.getInstance().sendMessage({ tag: 'DataSyncQuery', query: this.query });
        return result;
    }

    async fetchOne() {
        const result = await this.limit(1).fetch();
        return result.length > 0 ? result[0] : null;
    }

    async fetchAndRefresh(callback) {
        const dataSubscription = new DataSubscription(this.query);
        let records = null;

        dataSubscription.onReady = result => { records = result; callback(result) };
        dataSubscription.onUpdate = (id, changeSet) => {
            for (const record of records) {
                if (record.id === id) {
                    Object.assign(record, changeSet);
                    break;
                }
            }

            callback(records);
        }
        dataSubscription.onCreate = newRecord => {
            records.push(newRecord);
            callback(records);
        };
        dataSubscription.onDelete = id => {
            records = records.filter(record => record.id !== id);
            callback(records);
        };
    }
}

function query(table) {
    return new QueryBuilder(table);
}

async function createRecord(table, payload) {
    const response = await fetchAuthenticated(`/api/${table}`, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: 'POST',
        body: JSON.stringify(payload)
    });

    // We duplicate this code here across all functions
    // to improve the call stack in errors inside the dev console
    //
    // E.g. now it shows:
    //
    // Uncaught (in promise) Error: new row violates row-level security policy for table "tasks" at createRecord
    //
    // The alternative would be:
    //
    // Uncaught (in promise) Error: new row violates row-level security policy for table "tasks" at parseResponse
    //
    // Which is not helpful as 'parseResponse' is an implementation detail
    if (response.ok) {
        return response.json();
    } else {
        if (response.status === 400) {
            const payload = await response.json();
            throw new Error(payload.errorMsg);
        }
        
        throw new Error(response.statusText);
    }
}

async function createRecords(table, records) {
    if (!Array.isArray(records)) {
        throw new Error('createRecords: Expected array of records');
    }
    const response = await fetchAuthenticated(`/api/${table}`, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: 'POST',
        body: JSON.stringify(records)
    })
    
    if (response.ok) {
        return response.json();
    } else {
        if (response.status === 400) {
            const payload = await response.json();
            throw new Error(payload.errorMsg);
        }
        
        throw new Error(response.statusText);
    }
}

async function updateRecordById(table, id, patch) {
    const response = await fetchAuthenticated(`/api/${table}/${id}`, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: 'PATCH',
        body: JSON.stringify(patch)
    });
    
    if (response.ok) {
        return response.json();
    } else {
        if (response.status === 400) {
            const payload = await response.json();
            throw new Error(payload.errorMsg);
        }
        
        throw new Error(response.statusText);
    }
}

async function deleteRecordById(table, id, patch) {
    const response = await fetchAuthenticated(`/api/${table}/${id}`, {
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        method: 'DELETE',
        body: JSON.stringify(patch)
    });
    
    if (response.ok) {
        return response.json();
    } else {
        if (response.status === 400) {
            const payload = await response.json();
            throw new Error(payload.errorMsg);
        }
        
        throw new Error(response.statusText);
    }
}

export { QueryBuilder, query, createRecord, createRecords, updateRecordById, deleteRecordById, ihpBackendUrl, fetchAuthenticated };