# Changelog for `ihp-schema-compiler`

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
