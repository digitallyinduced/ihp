import { DataSyncController } from './ihp-datasync.js';
import { DataSubscriptionStore } from './data-subscription-store.js';
import type { ConditionExpression, ConditionOperator, DynamicSQLQuery, DataRecord, UUID, TableName, IHPRecord } from './types.js';

function fetchAuthenticated(path: string, params: RequestInit & { headers?: Record<string, string> }): Promise<Response> {
    const jwt = localStorage.getItem('ihp_jwt');
    if (jwt !== null) {
        params.headers = params.headers || {};
        params.headers['Authorization'] = 'Bearer ' + jwt;
    } else {
        params.credentials = 'include';
    }

    return fetch(ihpBackendUrl(path), params);
}

function ihpBackendUrl(path: string): string {
    const host = DataSyncController.ihpBackendHost || '';
    return host + path;
}

function infixMany(op: ConditionOperator, args: ConditionExpression[] | [ConditionExpression[]]): ConditionExpression {
    const flatArgs: ConditionExpression[] = (typeof (args[0] as ConditionExpression[]).length === 'number') ? args[0] as ConditionExpression[] : args as ConditionExpression[];
    const [left, right, ...rest] = flatArgs;
    const lr: ConditionExpression = {
        tag: 'InfixOperatorExpression',
        left,
        op,
        right,
    };

    if (rest.length === 0) {
        return lr;
    }

    return infixMany(op, [lr, ...rest]);
}

function and(...args: ConditionExpression[] | [ConditionExpression[]]): ConditionExpression {
    return infixMany('OpAnd', args);
}

function or(...args: ConditionExpression[] | [ConditionExpression[]]): ConditionExpression {
    return infixMany('OpOr', args);
}

function undefinedToNull(value: unknown): unknown {
    return value === undefined ? null : value;
}

function infixColumnLiteral(field: string, op: ConditionOperator, value: unknown): ConditionExpression {
    return {
        tag: 'InfixOperatorExpression',
        left: {
            tag: 'ColumnExpression',
            field,
        },
        op,
        right: {
            tag: 'LiteralExpression',
            value: undefinedToNull(value),
        },
    };
}

function eq(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpEqual', value);
}

function notEq(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpNotEqual', value);
}

function lessThan(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpLessThan', value);
}

function lessThanOrEqual(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpLessThanOrEqual', value);
}

function greaterThan(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpGreaterThan', value);
}

function greaterThanOrEqual(field: string, value: unknown): ConditionExpression {
    return infixColumnLiteral(field, 'OpGreaterThanOrEqual', value);
}

// Free-standing condition builder functions - these create a new ConditionBuilder and delegate
function where(field: string, value: unknown): ConditionBuilder;
function where(conditionBuilder: ConditionBuilder): ConditionBuilder;
function where(filterRecord: DataRecord): ConditionBuilder;
function where(...args: unknown[]): ConditionBuilder {
    return new ConditionBuilder().where(...args as [string, unknown]);
}

function whereNot(field: string, value: unknown): ConditionBuilder {
    return new ConditionBuilder().whereNot(field, value);
}

function whereLessThan(field: string, value: unknown): ConditionBuilder {
    return new ConditionBuilder().whereLessThan(field, value);
}

function whereLessThanOrEqual(field: string, value: unknown): ConditionBuilder {
    return new ConditionBuilder().whereLessThanOrEqual(field, value);
}

function whereGreaterThan(field: string, value: unknown): ConditionBuilder {
    return new ConditionBuilder().whereGreaterThan(field, value);
}

function whereGreaterThanOrEqual(field: string, value: unknown): ConditionBuilder {
    return new ConditionBuilder().whereGreaterThanOrEqual(field, value);
}

function filterWhere(field: string, value: unknown): ConditionBuilder {
    return where(field, value);
}

function whereIn(field: string, values: unknown[]): ConditionBuilder {
    return new ConditionBuilder().whereIn(field, values);
}

class ConditionBuildable<TTable extends string = string> {
    hasPreviousCondition: boolean;
    query: { whereCondition: ConditionExpression | null };

    constructor(initialWhereCondition: ConditionExpression | null) {
        this.hasPreviousCondition = false;
        this.query = {
            whereCondition: initialWhereCondition,
        };
    }

    _addCondition(operator: ConditionOperator, conditionExpression: ConditionExpression): void {
        if (!this.hasPreviousCondition) {
            this.query.whereCondition = conditionExpression;
            this.hasPreviousCondition = true;
        } else {
            this.query.whereCondition = {
                tag: 'InfixOperatorExpression',
                left: this.query.whereCondition!,
                op: operator,
                right: conditionExpression,
            };
        }
    }

    where<K extends keyof IHPRecord<TTable> & string>(column: K, value: IHPRecord<TTable>[K]): this;
    where(conditionBuilder: ConditionBuilder): this;
    where(filterRecord: Partial<IHPRecord<TTable>>): this;
    where(conditionExpression: ConditionExpression): this;
    where(...args: unknown[]): this {
        switch (args.length) {
        case 2:
            if (typeof args[0] !== 'string') {
                throw Error('When calling .where(..) with two arguments, the first has to be a column name as a string. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference');
            }

            // this is a shorthand case: where(field, value) === where(eq(field, value))
            return this.where(eq(args[0], args[1]));
        case 1:
            if (typeof args[0] !== 'object') {
                if (typeof args[0] === 'string') {
                    throw new Error('You called .where(..) with only one argument that is a string. Did you mean to add a second parameter to add an equality condition?');
                }

                throw new Error('You called .where(..) with only one argument, but I don\'d understand its type. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference');
            }

            if ((args[0] as ConditionBuilder).type === 'ConditionBuilder') {
                // this is the full case: where(<ConditionBuilder>)
                const conditionBuilder = args[0] as ConditionBuilder;
                return this.where(conditionBuilder.query.whereCondition!);
            }

            if (!Object.prototype.hasOwnProperty.call(args[0], 'tag')) {
                // received a key-value object with conditions: query(..).where({ field: value, field2: value2 })
                // turns it into: query(..).where(and([eq(field, value), eq(field2, value2)]))
                return this.where(and(Object.entries(args[0] as DataRecord).map(([k, v]) => eq(k, v))));
            }

            // received an already compiled condition, for example via: where(eq(field, value))
            this._addCondition('OpAnd', args[0] as ConditionExpression);
            return this;
        default:
            throw new Error('.where(..) accepts only one or two arguments. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference');
        }
    }

    whereNot<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(notEq(field, value));
    }

    whereLessThan<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(lessThan(field, value));
    }

    whereLessThanOrEqual<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(lessThanOrEqual(field, value));
    }

    whereGreaterThan<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(greaterThan(field, value));
    }

    whereGreaterThanOrEqual<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(greaterThanOrEqual(field, value));
    }

    filterWhere<K extends keyof IHPRecord<TTable> & string>(field: K, value: IHPRecord<TTable>[K]): this {
        return this.where(field, value);
    }

    or(conditionBuilder: ConditionBuilder): this {
        if (!this.hasPreviousCondition) {
            throw new Error('You are attempting to add a condition using `or`, but there are no previous conditions to apply the `or` to. You probably want `where` instead.');
        }
        const conditionExpression = conditionBuilder.query.whereCondition!;
        this._addCondition('OpOr', conditionExpression);
        return this;
    }

    and(conditionBuilder: ConditionBuilder): this {
        if (!this.hasPreviousCondition) {
            throw new Error('You are attempting to add a condition using `and`, but there are no previous conditions to apply the `and` to. You probably want `where` instead.');
        }
        const conditionExpression = conditionBuilder.query.whereCondition!;
        this._addCondition('OpAnd', conditionExpression);
        return this;
    }

    whereIn<K extends keyof IHPRecord<TTable> & string>(field: K, values: Array<IHPRecord<TTable>[K]>): this {
        const expression: ConditionExpression = {
            tag: 'InfixOperatorExpression',
            left: {
                tag: 'ColumnExpression',
                field,
            },
            op: 'OpIn',
            right: {
                tag: 'ListExpression',
                values: (values as unknown[]).map(undefinedToNull),
            },
        };
        this._addCondition('OpAnd', expression);
        return this;
    }
}

class ConditionBuilder extends ConditionBuildable {
    type: 'ConditionBuilder';

    constructor() {
        super({
            tag: 'LiteralExpression',
            value: true,
        });
        this.type = 'ConditionBuilder';
    }
}

class QueryBuilder<TTable extends string = string, TResult = IHPRecord<TTable>> extends ConditionBuildable<TTable> {
    override query: DynamicSQLQuery;
    transactionId: UUID | null;
    private readonly dataSyncController: DataSyncController | null;

    constructor(table: string, columns?: string[], dataSyncController: DataSyncController | null = null) {
        super(null);
        // Maps to 'DynamicSQLQuery'
        this.query = {
            table,
            selectedColumns: { tag: 'SelectAll' },
            whereCondition: null,
            orderByClause: [],
            distinctOnColumn: null,
            limit: null,
            offset: null
        };
        this.transactionId = null;
        this.dataSyncController = dataSyncController;

        if (columns !== undefined) {
            this.select(columns);
        }
    }

    select(...columns: (string | string[])[]): this {
        if (columns.length > 1) {
            for (const column of columns) {
                this.select(column);
            }
            return this;
        }

        if (typeof columns[0] === 'object') {
            return this.select(...columns[0]);
        }

        if (this.query.selectedColumns.tag === 'SelectAll') {
            this.query.selectedColumns = { tag: 'SelectSpecific', contents: [] };
        }

        const column = columns[0] as string;

        if (this.query.selectedColumns.tag === 'SelectSpecific' && !this.query.selectedColumns.contents.includes(column)) {
            this.query.selectedColumns.contents.push(column);
        }

        return this;
    }

    whereTextSearchStartsWith(field: keyof IHPRecord<TTable> & string, value: string): this {
        let normalized = String(value ?? '').trim().split(' ').map(s => s.trim()).filter(v => v.length > 0).join('&');
        if (normalized.length > 0) {
            normalized += ':*';
        }
        const expression: ConditionExpression = {
            tag: 'InfixOperatorExpression',
            left: {
                tag: 'ColumnExpression',
                field,
            },
            op: 'OpTSMatch',
            right: {
                tag: 'CallExpression',
                functionCall: {
                    tag: 'ToTSQuery',
                    text: normalized
                }
            },
        };

        this._addCondition('OpAnd', expression);

        this.query.orderByClause.push({ tag: 'OrderByTSRank', tsvector: field, tsquery: normalized });

        return this;
    }

    orderBy(column: keyof IHPRecord<TTable> & string): this {
        return this.orderByAsc(column);
    }

    orderByAsc(column: keyof IHPRecord<TTable> & string): this {
        this.query.orderByClause.push({ orderByColumn: column, orderByDirection: 'Asc' });

        return this;
    }

    orderByDesc(column: keyof IHPRecord<TTable> & string): this {
        this.query.orderByClause.push({ orderByColumn: column, orderByDirection: 'Desc' });

        return this;
    }

    distinctOn(column: keyof IHPRecord<TTable> & string): this {
        this.query.distinctOnColumn = column;

        return this;
    }

    limit(limit: number | null): this {
        if (limit !== null && !Number.isInteger(limit)) {
            throw new Error('limit needs to be an integer, or null if no limit should be used');
        }
        this.query.limit = limit;

        return this;
    }

    offset(offset: number | null): this {
        if (offset !== null && !Number.isInteger(offset)) {
            throw new Error('offset needs to be an integer, or null if no offset should be used');
        }
        this.query.offset = offset;

        return this;
    }

    async fetch(): Promise<TResult[]> {
        // Transaction queries must stay on the controller that created their
        // transaction id. Its transport-scope guard rejects after auth/backend
        // rotation instead of leaking the id onto a replacement connection.
        const dataSyncController = this.dataSyncController ?? DataSyncController.getInstance();
        const response = await dataSyncController.sendMessage({
            tag: 'DataSyncQuery',
            query: this.query,
            transactionId: this.transactionId
        });

        return response.result as TResult[];
    }

    async fetchOne(): Promise<TResult | null> {
        const result = await this.limit(1).fetch();
        return result.length > 0 ? result[0] : null;
    }

    subscribe(callback: (records: TResult[] | null) => void): () => void {
        if (this.transactionId !== null || this.dataSyncController !== null) {
            throw new Error('DataSync subscriptions are not supported inside a transaction');
        }
        const dataSubscription = DataSubscriptionStore.get(this.query);
        return dataSubscription.subscribe(callback as (records: DataRecord[] | null) => void);
    }
}

function query<T extends TableName>(table: T, columns?: string[]): QueryBuilder<T, IHPRecord<T>> {
    return new QueryBuilder<T, IHPRecord<T>>(table, columns);
}

export function recordMatchesQuery(query: DynamicSQLQuery, record: DataRecord): boolean {
    type SQLBoolean = boolean | null;

    const isNull = (value: unknown): boolean => value === null || value === undefined;
    const asSQLBoolean = (value: unknown): SQLBoolean => isNull(value) ? null : Boolean(value);
    const sqlAnd = (left: unknown, right: unknown): SQLBoolean => {
        const leftBoolean = asSQLBoolean(left);
        const rightBoolean = asSQLBoolean(right);
        if (leftBoolean === false || rightBoolean === false) {
            return false;
        }
        return leftBoolean === null || rightBoolean === null ? null : true;
    };
    const sqlOr = (left: unknown, right: unknown): SQLBoolean => {
        const leftBoolean = asSQLBoolean(left);
        const rightBoolean = asSQLBoolean(right);
        if (leftBoolean === true || rightBoolean === true) {
            return true;
        }
        return leftBoolean === null || rightBoolean === null ? null : false;
    };

    function evaluate(expression: ConditionExpression): unknown {
        switch (expression.tag) {
            case 'ColumnExpression': return (expression.field in record) ? record[expression.field] : null;
            case 'InfixOperatorExpression': {
                const left = evaluate(expression.left);
                const right = evaluate(expression.right);
                switch (expression.op) {
                    case 'OpEqual':
                        if (expression.right.tag === 'LiteralExpression' && isNull(right)) {
                            return isNull(left);
                        }
                        return isNull(left) || isNull(right) ? null : left === right;
                    case 'OpGreaterThan': return isNull(left) || isNull(right) ? null : (left as number) > (right as number);
                    case 'OpLessThan': return isNull(left) || isNull(right) ? null : (left as number) < (right as number);
                    case 'OpGreaterThanOrEqual': return isNull(left) || isNull(right) ? null : (left as number) >= (right as number);
                    case 'OpLessThanOrEqual': return isNull(left) || isNull(right) ? null : (left as number) <= (right as number);
                    case 'OpNotEqual':
                        if (expression.right.tag === 'LiteralExpression' && isNull(right)) {
                            return !isNull(left);
                        }
                        return isNull(left) || isNull(right) ? null : left !== right;
                    case 'OpAnd': return sqlAnd(left, right);
                    case 'OpOr': return sqlOr(left, right);
                    case 'OpIs': return isNull(left) && isNull(right) || !isNull(left) && !isNull(right) && left === right;
                    case 'OpIsNot': return !(isNull(left) && isNull(right) || !isNull(left) && !isNull(right) && left === right);
                    case 'OpIn': {
                        if (!Array.isArray(right)) {
                            return false;
                        }
                        if (isNull(left)) {
                            return right.some(isNull) ? true : null;
                        }
                        return right.filter(value => !isNull(value)).includes(left);
                    }
                    // Full-text matching depends on PostgreSQL dictionaries and stemming
                    // and cannot be reproduced exactly in this standalone helper.
                    case 'OpTSMatch': return false;
                    default: return false;
                }
            }
            case 'LiteralExpression': return expression.value;
            case 'ListExpression': return expression.values;
            case 'CallExpression': return false;
            default: return false;
        }
    }

    return query.whereCondition === null
            ? true
            : evaluate(query.whereCondition) === true;
}

export {
    QueryBuilder,
    ConditionBuilder,
    ConditionBuildable,
    query,
    ihpBackendUrl,
    fetchAuthenticated,
    filterWhere,
    where,
    whereNot,
    eq,
    notEq,
    or,
    and,
    lessThan,
    lessThanOrEqual,
    greaterThan,
    greaterThanOrEqual,
    whereLessThan,
    whereLessThanOrEqual,
    whereGreaterThan,
    whereGreaterThanOrEqual,
    whereIn,
};
