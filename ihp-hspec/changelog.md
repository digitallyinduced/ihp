# Changelog for `ihp-hspec`

## v1.6.0

- Add response assertion helpers: `responseBodyShouldContain`,
  `responseBodyShouldNotContain`, and `responseStatusShouldBe`
- Replace the `ihp-log` dependency with the framework `noopLogger`

## v1.5.0

- Migrate from postgresql-simple to hasql
- Run full middleware stack for each test request
- Add bracket-style `withModelContext`

## v1.4.0

- Initial release as a standalone package
