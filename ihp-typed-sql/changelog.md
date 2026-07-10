# Changelog for `ihp-typed-sql`

## Unreleased

- The automatic compile-time PostgreSQL database now reuses one cache under
  the worktree's `.devenv/state` directory. A detached idle supervisor stops
  PostgreSQL after metadata operations and recovers abandoned compiler locks,
  preventing temporary clusters and orphaned server processes from accumulating.

- Added `sqlQueryTypedPipelined` for running typed SQL queries through
  `IHP.FetchPipelined.pipeline`.

- Added explicit cardinality helpers: `sqlQueryTypedRows`,
  `sqlQueryTypedOneOrNothing`, and `sqlQueryTypedSingle`.

- Added `sqlQueryTypedMaybeColumn` and `sqlQueryTypedMaybeColumnPipelined` for
  nullable single-column `LIMIT 1` queries where callers want both "no row" and
  "SQL NULL" represented as `Nothing`.

- `typedSql` now infers `json_build_object`, `jsonb_build_object`,
  `json_build_array`, and `jsonb_build_array` as non-null computed JSON
  expressions.

- `sqlExecTyped` now also supports known typed no-result utility statements
  such as `SET CONSTRAINTS ...`, returning `()` after successful execution.

## v1.7.0

- **Breaking:** `TypedQuery` now carries type-level cardinality and statement
  result markers. `sqlQueryTyped` returns a shape derived from the cardinality:
  many-row queries still return `[result]`; queries proven to return at most one
  row return `Maybe result`; queries proven to return exactly one row return
  `result` directly. The quasiquoter currently detects conservative cases such
  as `LIMIT 1`, primary key lookups, singleton `VALUES`, aggregate queries
  without `GROUP BY`, no-`FROM` singleton selects, and simple singleton
  CTE/subquery projections.

- Added `paginatedTypedSql` and `paginatedTypedSqlWithOptions` (in the new
  `IHP.TypedSql.Pagination` module). These are the `typedSql` analogue of IHP's
  `paginatedSqlQuery` / `paginatedSqlQueryWithOptions`: pass a many-row
  row-returning `TypedQuery` and get back `([model], Pagination)`, with the
  same `page` / `maxItems` request params, the same 200-item cap, and the same
  `Pagination` shape. Put any `ORDER BY` inside the query you pass in — the
  query is wrapped in a subquery before `LIMIT` / `OFFSET` are applied.

## v1.6.0

- Multi-column queries now generate named `SqlRow` result types with record-dot
  field access.

- More expressions are inferred as non-nullable when PostgreSQL guarantees a
  value, including `COUNT`, `EXISTS`, `row_number`, `rank`, `dense_rank`,
  non-null literals, and `COALESCE` / casts with non-null inputs.

- `typedSql` rejects `SELECT *` and `INSERT` statements without explicit column
  lists by default. This catches schema-drift bugs at compile time instead of in
  production.

- `typedSql` can now start a temporary private PostgreSQL instance for
  compile-time query description when `DATABASE_URL` is unreachable and
  `IHP_TYPED_SQL_AUTO_DB=1` is set. IHP app dev shells enable this by default,
  which makes non-interactive agent/workspace typechecking work without a
  separate `devenv up`.

- Better error message when a `${...}` placeholder appears in a polymorphic-argument
  position (e.g. `CONCAT`, `COALESCE`, `GREATEST`, `LEAST`). Instead of surfacing
  the raw Postgres `could not determine data type of parameter $N` text,
  `typedSql` now points at the offending `${expr}` and suggests an explicit
  `${expr}::text` cast (#2667).

- **Breaking:** `int8` / `bigint` columns and placeholders now map to `Int64`
  instead of `Integer`, exactly matching PostgreSQL's 64-bit wire format.
  Migration: search for `Integer` values flowing through `typedSql` results
  (e.g. `COUNT(*)`, `row_number()`, `bigint` columns) and replace with
  `Int64`. Callers holding `Int` values (e.g. from `limit` / `offset` in the
  query builder) still need `fromIntegral` when passing into `typedSql`
  placeholders, but the conversion target is now precise rather than
  arbitrary-precision.

## v1.5.0

- Improve error messages and developer experience
- Fix LEFT/RIGHT JOIN nullability detection
- Preserve case for quoted identifiers in SQL parser
- Replace hand-rolled SQL tokenizer with `postgresql-syntax` parser
- Fix int2 decoder bug and connection leaks
- Add granular runtime integration tests
- Remove bootstrap mode

## v1.4.0

- Initial release as a standalone package
