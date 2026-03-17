# Changelog for `ihp-datasync`

## v1.5.0

- Migrate from postgresql-simple to hasql with prepared statements
- Use hasql for RLS queries instead of falling back to postgresql-simple
- Replace `DynamicValue` with Aeson `Value` for simpler JSON handling
- Send append-only deltas for streaming text updates
- Fix `CountSubscription` comparing against initial count instead of last-sent count
- Fix DataSync trigger installation deadlock
- Return HTTP 400 JSON responses instead of crashing on invalid `requestBodyJSON`
- Use `postgresql-types` (`Point`, `Polygon`, `Interval`, `Inet`) instead of custom types
- Schema-aware typed parameter encoding
- Add end-to-end integration tests against real PostgreSQL

## v1.4.0

- Initial release as a standalone package, extracted from ihp
