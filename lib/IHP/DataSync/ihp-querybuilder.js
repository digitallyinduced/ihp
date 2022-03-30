import { DataSyncController, DataSubscription } from './ihp-datasync.js';

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

function infixMany(op, args) {
    const [left, right, ...rest] = typeof args[0].length === 'number' ? args[0] : args
    const lr = {
        tag: 'InfixOperatorExpression',
        left,
        op,
        right,
    }

    if (rest.length === 0) {
        return lr
    }

    return infixMany.call(this, op, [lr, ...rest])
}

function and(...args) {
    return infixMany('OpAnd', args)
}

function or(...args) {
    return infixMany('OpOr', args)
}

function infixColumnLiteral(field, op, value) {
    return {
        tag: 'InfixOperatorExpression',
        left: {
            tag: 'ColumnExpression',
            field,
        },
        op,
        right: {
            tag: 'LiteralExpression',
            value: jsValueToDynamicValue(value),
        },
    }
}

function eq(field, value) {
    return infixColumnLiteral(field, 'OpEqual', value)
}

function notEq(field, value) {
    return infixColumnLiteral(field, 'OpNotEqual', value)
}

function lessThan(field, value) {
    return infixColumnLiteral(field, 'OpLessThan', value)
}

function lessThanOrEqual(field, value) {
    return infixColumnLiteral(field, 'OpLessThanOrEqual', value)
}

function greaterThan(field, value) {
    return infixColumnLiteral(field, 'OpGreaterThan', value)
}

function greaterThanOrEqual(field, value) {
    return infixColumnLiteral(field, 'OpGreaterThanOrEqual', value)
}

function where(...args) {
    return (new ConditionBuilder()).where(...args)
}

function whereNot(...args) {
    return (new ConditionBuilder()).whereNot(...args)
}

function whereLessThan(...args) {
    return (new ConditionBuilder()).whereLessThan(...args)
}

function whereLessThanOrEqual(...args) {
    return (new ConditionBuilder()).whereLessThanOrEqual(...args)
}

function whereGreaterThan(...args) {
    return (new ConditionBuilder()).whereGreaterThan(...args)
}

function whereGreaterThanOrEqual(...args) {
    return (new ConditionBuilder()).whereGreaterThanOrEqual(...args)
}

function filterWhere(field, value) {
    return where(field, value)
}

class ConditionBuildable {
    constructor(initialWhereCondition) {
        this.hasPreviousCondition = false;
        this.query = {
            whereCondition: initialWhereCondition,
        };
    }

    _addCondition(operator, conditionExpression) {
        if (!this.hasPreviousCondition) {
            this.query.whereCondition = conditionExpression;
            this.hasPreviousCondition = true;
        } else {
            this.query.whereCondition = {
                tag: 'InfixOperatorExpression',
                left: this.query.whereCondition,
                op: operator,
                right: conditionExpression,
            };
        }
    }

    where(...args) {
        switch (args.length) {
        case 2:
            if (typeof args[0] !== 'string') {
                throw Error('When calling .where(..) with two arguments, the first has to be a column name as a string. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference')
            }

            // this is a shorthand case: where(field, value) === where(eq(field, value))
            return this.where(eq(args[0], args[1]))
        case 1:
            if (typeof args[0] !== 'object') {
                if (typeof args[0] === 'string') {
                    throw new Error('You called .where(..) with only one argument that is a string. Did you mean to add a second parameter to add an equality condition?')
                }

                throw new Error('You called .where(..) with only one argument, but I don\'d understand its type. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference')
            }

            if (args[0].type === 'ConditionBuilder') {
                // this is the full case: where(<ConditionBuilder>)
                const conditionBuilder = args[0];
                return this.where(conditionBuilder.query.whereCondition)
            }

            if (!args[0].hasOwnProperty('tag')) {
                // received a key-value object with conditions: query(..).where({ field: value, field2: value2 })
                // turns it into: query(..).where(and([eq(field, value), eq(field2, value2)]))
                return this.where(and(Object.entries(args[0]).map(([k, v]) => eq(k, v))))
            }

            // received an already compiled condition, for example via: where(eq(field, value))
            this._addCondition('OpAnd', args[0])
            return this;
        default:
            throw new Error('.where(..) accepts only one or two arguments. Read more here: https://ihpbackend.digitallyinduced.com/docs/api-reference')
        }
    }

    whereNot(field, value) {
        return this.where(notEq(field, value))
    }

    whereLessThan(field, value) {
        return this.where(lessThan(field, value))
    }

    whereLessThanOrEqual(field, value) {
        return this.where(lessThanOrEqual(field, value))
    }

    whereGreaterThan(field, value) {
        return this.where(greaterThan(field, value))
    }

    whereGreaterThanOrEqual(field, value) {
        return this.where(greaterThanOrEqual(field, value))
    }

    filterWhere(field, value) {
        return this.where(field, value)
    }

    or(conditionBuilder) {
        if (!this.hasPreviousCondition) {
            throw new Error('You are attempting to add a condition using `or`, but there are no previous conditions to apply the `or` to. You probably want `where` instead.')
        }
        const conditionExpression = conditionBuilder.query.whereCondition;
        this._addCondition('OpOr', conditionExpression)
        return this
    }

    and(conditionBuilder) {
        if (!this.hasPreviousCondition) {
            throw new Error('You are attempting to add a condition using `and`, but there are no previous conditions to apply the `and` to. You probably want `where` instead.')
        }
        const conditionExpression = conditionBuilder.query.whereCondition;
        this._addCondition('OpAnd', conditionExpression)
        return this
    }
}

class ConditionBuilder extends ConditionBuildable {
    constructor() {
        super({
            tag: 'LiteralExpression',
            value: jsValueToDynamicValue(true),
        })
        this.type = 'ConditionBuilder';
    }
}

class QueryBuilder extends ConditionBuildable {
    constructor(table, columns) {
        super(null)
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

        if (columns !== undefined) {
            this.select(columns)
        }
    }

    select(...columns) {
        if (columns.length > 1) {
            for (let column of columns) {
                this.select(column)
            }
            return this
        }

        if (typeof columns[0] === 'object') {
            return this.select(...columns[0])
        }

        if (this.query.selectedColumns.tag === 'SelectAll') {
            this.query.selectedColumns = { tag: 'SelectSpecific', contents: [] }
        }

        const column = columns[0]

        if (!this.query.selectedColumns.contents.includes(column)) {
            this.query.selectedColumns.contents.push(column)
        }

        return this
    }

    whereTextSearchStartsWith(field, value) {
        let normalized = value.trim().split(' ').map(s => s.trim()).filter(v => v.length > 0).join('&');
        if (normalized.length > 0) {
            normalized +=  ':*';
        }
        const expression = {
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

    distinctOn(column) {
        this.query.distinctOnColumn = column

        return this
    }

    limit(limit) {
        if (limit !== null && !Number.isInteger(limit)) {
            throw new Error('limit needs to be an integer, or null if no limit should be used');
        }
        this.query.limit = limit;

        return this;
    }

    offset(offset) {
        if (offset !== null && !Number.isInteger(offset)) {
            throw new Error('offset needs to be an integer, or null if no offset should be used');
        }
        this.query.offset = offset;

        return this;
    }

    async fetch() {
        const { result } = await DataSyncController.getInstance().sendMessage({
            tag: 'DataSyncQuery',
            query: this.query,
            transactionId: this.transactionId
        });
        return result;
    }

    async fetchOne() {
        const result = await this.limit(1).fetch();
        return result.length > 0 ? result[0] : null;
    }

    subscribe(callback) {
        const dataSubscription = new DataSubscription(this.query);
        dataSubscription.createOnServer();
        return dataSubscription.subscribe(callback)
    }
}

function query(table) {
    return new QueryBuilder(table);
}

function jsValueToDynamicValue(value) {
    if (typeof value === "string") {
        return { tag: 'TextValue', contents: value };
    } else if (typeof value === "number") {
        return { tag: Number.isInteger(value) ? 'IntValue' : 'DoubleValue', contents: value };
    } else if (typeof value === "boolean") {
        return { tag: 'BoolValue', contents: value };
    } else if (value === null || value === undefined) {
        return { tag: 'Null' };
    }

    throw new Error('Could no transform JS value to DynamicValue. Supported types: string, number, boolean, null, undefined');
}

export function recordMatchesQuery(query, record) {
    function evaluate(expression) {
        switch (expression.tag) {
            case 'ColumnExpression': return (expression.field in record) ? record[expression.field] : null;
            case 'InfixOperatorExpression': {
                switch (expression.op) {
                    case 'OpEqual': return evaluate(expression.left) == evaluate(expression.right);
                    case 'OpGreaterThan': return evaluate(expression.left) > evaluate(expression.right);
                    case 'OpLessThan': return evaluate(expression.left) < evaluate(expression.right);
                    case 'OpGreaterThanOrEqual': return evaluate(expression.left) >= evaluate(expression.right);
                    case 'OpLessThanOrEqual': return evaluate(expression.left) <= evaluate(expression.right);
                    case 'OpNotEqual': return evaluate(expression.left) != evaluate(expression.right);
                    case 'OpAnd': return evaluate(expression.left) && evaluate(expression.right);
                    case 'OpOr': return evaluate(expression.left) || evaluate(expression.right);
                    case 'OpIs': return evaluate(expression.left) == evaluate(expression.right);
                    case 'OpIsNot': return evaluate(expression.left) != evaluate(expression.right);
                    default: throw new Error('Unsupported operator ' + expression.op);
                }
            }
            case 'NullExpression': return null;
            case 'LiteralExpression': return (expression.value.tag === 'Null' ? null : expression.value.contents);
            default: throw new Error('Unsupported expression in evaluate: ' + expression.tag);
        }
    }

    return query.whereCondition === null
            ? true
            : evaluate(query.whereCondition);
}

export {
    QueryBuilder,
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
};
