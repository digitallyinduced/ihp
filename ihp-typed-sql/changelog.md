# Changelog for `ihp-typed-sql`

## Unreleased

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
