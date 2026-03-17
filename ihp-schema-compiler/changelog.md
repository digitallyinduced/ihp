# Changelog for `ihp-schema-compiler`

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
