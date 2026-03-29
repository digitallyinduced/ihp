# Changelog for `ihp-typed-sql`

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
