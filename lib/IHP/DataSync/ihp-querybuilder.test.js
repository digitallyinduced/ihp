import {
    query,
    where,
    filterWhere,
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
} from './ihp-querybuilder';

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
    greaterThan: 'OpGreaterThan',
    lessThan: 'OpLessThan',
    greaterThanOrEqual: 'OpGreaterThanOrEqual',
    lessThanOrEqual: 'OpLessThanOrEqual',
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
	['whereLessThan', b => b.build(), operators.lessThan, whereLessThan],
	['whereLessThanOrEqual', b => b.build(), operators.lessThanOrEqual, whereLessThanOrEqual],
	['whereGreaterThan', b => b.build(), operators.greaterThan, whereGreaterThan],
	['whereGreaterThanOrEqual', b => b.build(), operators.greaterThanOrEqual, whereGreaterThanOrEqual],
	['eq', b => b, operators.equal, eq],
	['notEq', b => b, operators.notEqual, notEq],
	['lessThan', b => b, operators.lessThan, lessThan],
	['lessThanOrEqual', b => b, operators.lessThanOrEqual, lessThanOrEqual],
	['greaterThan', b => b, operators.greaterThan, greaterThan],
	['greaterThanOrEqual', b => b, operators.greaterThanOrEqual, greaterThanOrEqual],
	['query(..).where', b => b.buildConditions(), operators.equal, queryBuilderConditionFn('where')],
	['query(..).filterWhere', b => b.buildConditions(), operators.equal, queryBuilderConditionFn('filterWhere')],
	['query(..).whereNot', b => b.buildConditions(), operators.notEqual, queryBuilderConditionFn('whereNot')],
    ].forEach(suite)
})

describe('ConditionBuilder', () => {
    it('has a function for every operator', () => {
	const builder = where('a', 1)
	      .whereNot('b', 2)
	      .whereLessThan('c', 3)
	      .whereLessThanOrEqual('d', 4)
	      .whereGreaterThan('e', 5)
	      .whereGreaterThanOrEqual('f', 6)

	expect(builder.build()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.infixOperator(
			expression.infixOperator(
			    expression.infixOperator(
				expression.column('a'),
				operators.equal,
				expression.literalInt(1)
			    ),
			    operators.and,
			    expression.infixOperator(
				expression.column('b'),
				operators.notEqual,
				expression.literalInt(2)
			    ),
			),
			operators.and,
			expression.infixOperator(
			    expression.column('c'),
			    operators.lessThan,
			    expression.literalInt(3)
			),
		    ),
		    operators.and,
		    expression.infixOperator(
			expression.column('d'),
			operators.lessThanOrEqual,
			expression.literalInt(4)
		    ),
		),
		operators.and,
		expression.infixOperator(
		    expression.column('e'),
		    operators.greaterThan,
		    expression.literalInt(5)
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('f'),
		operators.greaterThanOrEqual,
		expression.literalInt(6)
	    ),
	))
    })

    it('has a function for every operator - filter...', () => {
	const builder = filterWhere('a', 1)

	expect(builder.build()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.equal,
	    expression.literalInt(1),
	))
    })
})

describe('QueryBuilder', () => {
    it('no conditions => whereConditions are null', () => {
	const builder = query('todos')
	expect(builder.buildConditions()).toBe(null)
    })

    it('query(..).whereNot works correctly', () => {
	const builder = query('todos')
	    .whereNot('a', 1)
	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
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
	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
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
	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
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

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
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

    it('query(..).where(eq(..))', () => {
	const builder = query('table')
	      .where(eq('a', 1))

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.equal,
	    expression.literalInt(1),
	))
    })

    it('query(..).filterWhere(eq(..))', () => {
	const builder = query('table')
	      .filterWhere(eq('a', 1))

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.equal,
	    expression.literalInt(1),
	))
    })

    it('query(..).where(notEq(..))', () => {
	const builder = query('table')
	      .where(notEq('a', 1))

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.notEqual,
	    expression.literalInt(1),
	))
    })

    it('query(..).filterWhere(notEq(..))', () => {
	const builder = query('table')
	      .filterWhere(notEq('a', 1))

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.notEqual,
	    expression.literalInt(1),
	))
    })

    it('query(..).where(or(eq(..), eq(..)))', () => {
	const builder = query('table')
	      .where(
		  or(
		      eq('a', 1),
		      eq('b', 2)
		  )
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.column('a'),
		operators.equal,
		expression.literalInt(1),
	    ),
	    operators.or,
	    expression.infixOperator(
		expression.column('b'),
		operators.equal,
		expression.literalInt(2),
	    ),
	))
    })

    it('query(..).where(and(eq(..), notEq(..)))', () => {
	const builder = query('table')
	      .where(
		  and(
		      eq('a', 1),
		      notEq('b', 2)
		  )
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
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
	))
    })

    it('query(..).where(or(eq(..), eq(..), eq(..)))', () => {
	const builder = query('table')
	      .where(
		  or(
		      eq('a', 1),
		      eq('b', 2),
		      eq('c', 3),
		  )
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.column('a'),
		    operators.equal,
		    expression.literalInt(1),
		),
		operators.or,
		expression.infixOperator(
		    expression.column('b'),
		    operators.equal,
		    expression.literalInt(2),
		),
	    ),
	    operators.or,
	    expression.infixOperator(
		expression.column('c'),
		operators.equal,
		expression.literalInt(3),
	    ),
	))
    })

    it('query(..).where(and(eq(..), eq(..), eq(..)))', () => {
	const builder = query('table')
	      .where(
		  and(
		      eq('a', 1),
		      eq('b', 2),
		      eq('c', 3),
		  )
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.column('a'),
		    operators.equal,
		    expression.literalInt(1),
		),
		operators.and,
		expression.infixOperator(
		    expression.column('b'),
		    operators.equal,
		    expression.literalInt(2),
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('c'),
		operators.equal,
		expression.literalInt(3),
	    ),
	))
    })

    it('query(..).where(or([eq(..), eq(..), eq(..),..]))', () => {
	const builder = query('table')
	      .where(
		  or([
		      eq('a', 1),
		      eq('b', 2),
		      eq('c', 3),
		  ])
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.column('a'),
		    operators.equal,
		    expression.literalInt(1),
		),
		operators.or,
		expression.infixOperator(
		    expression.column('b'),
		    operators.equal,
		    expression.literalInt(2),
		),
	    ),
	    operators.or,
	    expression.infixOperator(
		expression.column('c'),
		operators.equal,
		expression.literalInt(3),
	    ),
	))
    })

    it('query(..).where(and([eq(..), eq(..), eq(..),..]))', () => {
	const builder = query('table')
	      .where(
		  and([
		      eq('a', 1),
		      eq('b', 2),
		      eq('c', 3),
		  ])
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.column('a'),
		    operators.equal,
		    expression.literalInt(1),
		),
		operators.and,
		expression.infixOperator(
		    expression.column('b'),
		    operators.equal,
		    expression.literalInt(2),
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('c'),
		operators.equal,
		expression.literalInt(3),
	    ),
	))
    })

    it('query(..).where(and(or(eq(..), eq(..)), eq(..)))', () => {
	const builder = query('table')
	      .where(
		  and(
		      or(
			  eq('a', 1),
			  eq('b', 2)
		      ),
		      eq('c', 3)
		  )
	      )

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.column('a'),
		    operators.equal,
		    expression.literalInt(1),
		),
		operators.or,
		expression.infixOperator(
		    expression.column('b'),
		    operators.equal,
		    expression.literalInt(2),
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('c'),
		operators.equal,
		expression.literalInt(3),
	    ),
	))
    })

    it('query(..).where({ f: v, g: w })', () => {
	const builder = query('table')
	      .where({
		  'a': 1,
		  'b': 2,
	      })

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.column('a'),
		operators.equal,
		expression.literalInt(1),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('b'),
		operators.equal,
		expression.literalInt(2),
	    ),
	))
    })

    it('query(..).where* is supported', () => {
	const builder = query('table')
	      .where('a', 1)
	      .whereNot('b', 2)
	      .whereLessThan('c', 3)
	      .whereLessThanOrEqual('d', 4)
	      .whereGreaterThan('e', 5)
	      .whereGreaterThanOrEqual('f', 6)

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.infixOperator(
		expression.infixOperator(
		    expression.infixOperator(
			expression.infixOperator(
			    expression.infixOperator(
				expression.column('a'),
				operators.equal,
				expression.literalInt(1)
			    ),
			    operators.and,
			    expression.infixOperator(
				expression.column('b'),
				operators.notEqual,
				expression.literalInt(2)
			    ),
			),
			operators.and,
			expression.infixOperator(
			    expression.column('c'),
			    operators.lessThan,
			    expression.literalInt(3)
			),
		    ),
		    operators.and,
		    expression.infixOperator(
			expression.column('d'),
			operators.lessThanOrEqual,
			expression.literalInt(4)
		    ),
		),
		operators.and,
		expression.infixOperator(
		    expression.column('e'),
		    operators.greaterThan,
		    expression.literalInt(5)
		),
	    ),
	    operators.and,
	    expression.infixOperator(
		expression.column('f'),
		operators.greaterThanOrEqual,
		expression.literalInt(6)
	    ),
	))
    })

    it('query(..).filterWhere is supported', () => {
	const builder = query('table').filterWhere('a', 1)

	expect(builder.buildConditions()).toStrictEqual(expression.infixOperator(
	    expression.column('a'),
	    operators.equal,
	    expression.literalInt(1),
	))
    })

})
