import { query, where, filterWhere, whereNot, filterWhereNot } from './ihp-querybuilder';

const tags = {
    expression: {
	infixOperator: 'InfixOperatorExpression',
	column: 'ColumnExpression',
	literal: 'LiteralExpression',
    },
    value: {
	'text': 'TextValue',
	'integer': 'IntValue',
	'double': 'DoubleValue',
	'bool': 'BoolValue',
	'null': 'Null',
    },
}

const operators = {
    equal: 'OpEqual',
    notEqual: 'OpNotEqual',
    and: 'OpAnd',
    or: 'OpOr',
}

const expression = {
    infixOperator: (left, op, right) => {
	return {
	    tag: tags.expression.infixOperator,
	    left,
	    op,
	    right,
	}
    },
    column: (field) => {
	return {
	    tag: tags.expression.column,
	    field,
	}
    },
    literal: (value) => {
	return {
	    tag: tags.expression.literal,
	    value,
	}
    },
    literalInt: (value) => {
	return {
	    tag: tags.expression.literal,
	    value: {
		tag: tags.value.integer,
		contents: value,
	    },
	}
    }
}

describe('Value Transformations and basic use', () => {
    const suite = ([fnName, extractor, operator, where]) => {
	describe(fnName, () => {

	    function expectValueTransformsTo(jsVal, literal) {
		const builder = where('column', jsVal)
		expect(extractor(builder)).toStrictEqual(expression.infixOperator(
		    expression.column('column'),
		    operator,
		    expression.literal(literal),
		))
	    }

	    it('string/text', () => {
		expectValueTransformsTo('value', { tag: tags.value.text, contents: 'value' })
	    })

	    it('number/integer', () => {
		expectValueTransformsTo(1, { tag: tags.value.integer, contents: 1 })
	    })

	    it('number/double', () => {
		expectValueTransformsTo(3.141, { tag: tags.value.double, contents: 3.141 })
	    })

	    it('true/bool', () => {
		expectValueTransformsTo(true, { tag: tags.value.bool, contents: true })
	    })

	    it('false/bool', () => {
		expectValueTransformsTo(false, { tag: tags.value.bool, contents: false })
	    })

	    it('null/null', () => {
		expectValueTransformsTo(null, { tag: tags.value['null'] })
	    })

	    it('undefined/null', () => {
		expectValueTransformsTo(undefined, { tag: tags.value['null'] })
	    })

	})
    }

    function queryBuilderConditionFn(fnName) {
	return function () {
	    const qb = query('table')
	    return qb[fnName].apply(qb, Array.from(arguments))
	}
    }

    [
	['where', b => b.build(), operators.equal, where],
	['filterWhere', b => b.build(), operators.equal, filterWhere],
	['whereNot', b => b.build(), operators.notEqual, whereNot],
	['filterWhereNot', b => b.build(), operators.notEqual, filterWhereNot],
	['query(..).where', b => b.query.whereCondition, operators.equal, queryBuilderConditionFn('where')],
	['query(..).filterWhere', b => b.query.whereCondition, operators.equal, queryBuilderConditionFn('filterWhere')],
	['query(..).whereNot', b => b.query.whereCondition, operators.notEqual, queryBuilderConditionFn('whereNot')],
	['query(..).filterWhereNot', b => b.query.whereCondition, operators.notEqual, queryBuilderConditionFn('filterWhereNot')],
    ].forEach(suite)
})

describe('QueryBuilder', () => {
    it('no conditions => whereConditions are null', () => {
	const builder = query('todos')
	expect(builder.query.whereCondition).toBe(null)
    })

    it('query(..).whereNot works correctly', () => {
	const builder = query('todos')
	    .whereNot('a', 1)
	expect(builder.query.whereCondition).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.notEqual,
	    expression.literal({
		tag: tags.value.integer,
		contents: 1,
	    })
	))
    })

    it('query(..).where can chain with .where', () => {
	const builder = query('todos')
	    .where('a', 1)
	    .where('b', 2)
	expect(builder.query.whereCondition).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.column('a'),
		operators.equal,
		expression.literal({
		    tag: tags.value.integer,
		    contents: 1,
		}),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('b'),
		operators.equal,
		expression.literal({
		    tag: tags.value.integer,
		    contents: 2,
		}),
	    ),
	))
    })

    it("query(..).where('a', 1).or(where('b', 2))", () => {
	const builder = query('todos')
	      .where('a', 1)
	      .or(where('b', 2))
	expect(builder.query.whereCondition).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.column('a'),
		operators.equal,
		expression.literal({
		    tag: tags.value.integer,
		    contents: 1,
		}),
	    ),
	    operators.or,
	    expression.infixOperator(
		expression.column('b'),
		operators.equal,
		expression.literal({
		    tag: tags.value.integer,
		    contents: 2,
		})
	    ),
	))
    })

    it('complex query with chaining functions', () => {
	const builder = query('table')
	    .where('a', 1)
	    .whereNot('b', 2)
	    .or(where('c', 3).where('d', 4))
	    .and(where('e', 5).or(where('f', 6)))
	    .where('g', 7)

	expect(builder.query.whereCondition).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.infixOperator(
			expression.infixOperator(
			    expression.column('a'),
			    operators.equal,
			    expression.literalInt(1),
			),
			operators.and,
			expression.infixOperator(
			    expression.column('b'),
			    operators.notEqual,
			    expression.literalInt(2),
			),
		    ),
		    operators.or,
		    expression.infixOperator(
			expression.infixOperator(
			    expression.column('c'),
			    operators.equal,
			    expression.literalInt(3)
			),
			operators.and,
			expression.infixOperator(
			    expression.column('d'),
			    operators.equal,
			    expression.literalInt(4),
			),
		    ),
		),
		operators.and,
		expression.infixOperator(
		    expression.infixOperator(
			expression.column('e'),
			operators.equal,
			expression.literalInt(5),
		    ),
		    operators.or,
		    expression.infixOperator(
			expression.column('f'),
			operators.equal,
			expression.literalInt(6),
		    ),
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('g'),
		operators.equal,
		expression.literalInt(7),
	    ),
	))
    })
})
