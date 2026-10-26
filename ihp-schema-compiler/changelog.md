# Changelog for `ihp-schema-compiler`

## Unreleased

- Share schema-local name and enum lookups, and skip reverse-relationship work
  when relation support is disabled.
- Cache unchanged schema compilation using a SHA-256 manifest of every output.
  The schema, relation options and immutable Nix compiler identity determine the
  cache key. Missing, modified or corrupt outputs cause regeneration.
- Add native Hspec cache regression tests and a Haskell comparison benchmark:
  `nix run .#schema-compiler-benchmark -- /path/to/old/build-generated-code /path/to/Application/Schema.sql`.

## v1.6.0

- Fix generated `createMany` statements for tables where writable columns have
  database defaults, including `id` columns. Generated code now emits `DEFAULT`
  per row when a field was not touched and respects mixed touched-field sets.

## v1.5.0

- Add PostgreSQL table inheritance (`INHERITS`) support
- Generate hasql statement modules instead of snippet-based codegen
- Generate `DefaultParamEncoder` instances for enum array columns and `Maybe` enum types
- Use `record update syntax` for `SetField`/`UpdateField` codegen
- Suppress `-Wambiguous-fields` warnings in generated types
- Use `postgresql-types` (`Point`, `Polygon`, `Interval`, `Inet`, `Tsvector`) instead of custom IHP types
- Split `Generated.ActualTypes` into per-table modules for parallel compilation
- Use `OsPath` instead of `FilePath`

## v1.4.0

- Initial release as a standalone package, extracted from ihp-ide
