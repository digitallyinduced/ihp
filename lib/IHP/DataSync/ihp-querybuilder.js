import { DataSyncController } from './ihp-datasync.js';

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
    const host = DataSyncController.ihpBackendHost || '';
    return host + path;
}

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
        const left = { tag: 'ColumnExpression', field };
        const op = 'OpEqual';
        const right = { tag: 'LiteralExpression', value: jsValueToDynamicValue(value) }
        this.query.whereCondition = { tag: 'InfixOperatorExpression', left, op, right };

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
}

function query(table) {
    return new QueryBuilder(table);
}

function jsValueToDynamicValue(value) {
    if (typeof value === "string") {
        return { tag: 'TextValue', contents: value };
    } else if (typeof value === "number") {
        return { tag: 'IntValue', contents: value };
    } else if (typeof value === "boolean") {
        return { tag: 'BoolValue', contents: value };
    } else if (value === null || value === undefined) {
        return { tag: 'NullValue' };
    }

    throw new Error('Could no transform JS value to DynamicValue. Supported types: string, number, boolean, null, undefined');
}

export { QueryBuilder, query, ihpBackendUrl, fetchAuthenticated };