# IHP Changelog

## v1.5.0 (Unreleased)

320 commits since v1.4.0. 410 files changed, 14,403 insertions, 8,319 deletions.

### Breaking Changes

- Removed `RequestContext`, replaced with WAI request vault and `?respond` parameter ([#2215](https://github.com/digitallyinduced/ihp/pull/2215))
- Added `?request :: Request` implicit parameter alongside `?context` ([#2218](https://github.com/digitallyinduced/ihp/pull/2218))
- Moved `ActionType` to WAI request vault ([#2225](https://github.com/digitallyinduced/ihp/pull/2225))
- Removed legacy `addStyle` function — use `<style>` tags in HSX instead
- Switched `toSlug` to use the `slugger` package ([#2153](https://github.com/digitallyinduced/ihp/pull/2153))
- Migrated Bootstrap CSS classes from v4 to v5 conventions (`ml-*` → `ms-*`, `sr-only` → `visually-hidden`, etc.) ([#2242](https://github.com/digitallyinduced/ihp/pull/2242))
- Migrated Bootstrap data attributes (`data-toggle` → `data-bs-toggle`, etc.)
- Added `-Werror=incomplete-patterns` across all packages ([#2260](https://github.com/digitallyinduced/ihp/pull/2260))
- Deprecated Makefile targets — use `nix build` instead ([#2170](https://github.com/digitallyinduced/ihp/pull/2170))
- Replaced `ApplicationContext` with WAI request vault for AutoRefresh ([#2149](https://github.com/digitallyinduced/ihp/pull/2149))
- Used `OsPath` instead of `FilePath` across all packages ([#2246](https://github.com/digitallyinduced/ihp/pull/2246))

### New Features

- `customRoutes` and `customPathTo` for overriding individual AutoRoute actions ([#2236](https://github.com/digitallyinduced/ihp/pull/2236))
- Static route shortcut via `IHP.Static` module ([#2237](https://github.com/digitallyinduced/ihp/pull/2237))
- `filterWhereGreaterThan`, `filterWhereLessThan`, `filterWhereAtLeast`, `filterWhereAtMost` comparison filter functions ([#2178](https://github.com/digitallyinduced/ihp/pull/2178))
- `filterWhereCaseInsensitiveJoinedTable` ([#2166](https://github.com/digitallyinduced/ihp/pull/2166))
- Improved code generators with type-aware scaffolding ([#2240](https://github.com/digitallyinduced/ihp/pull/2240))
- `compileRelationSupport` flag to optionally disable relation type machinery for faster compilation ([#2255](https://github.com/digitallyinduced/ihp/pull/2255))
- Persistent socket for seamless app restarts during development ([#2210](https://github.com/digitallyinduced/ihp/pull/2210))
- DataSync: append-only deltas for streaming text updates ([#2265](https://github.com/digitallyinduced/ihp/pull/2265))
- DataSync: `columns` parameter for `query` function
- SSC: proper error handling with `SSCError` type, reconnection support, production-ready documentation ([#2224](https://github.com/digitallyinduced/ihp/pull/2224))
- `Fixtures.sql` is now optional ([#2235](https://github.com/digitallyinduced/ihp/pull/2235))
- `nix flake check --impure` support for IHP projects ([#2146](https://github.com/digitallyinduced/ihp/pull/2146))
- `autocapitalize` attribute support in HSX ([#2176](https://github.com/digitallyinduced/ihp/pull/2176))
- `EnvVarReader` instance for `SMTPEncryption` ([#2214](https://github.com/digitallyinduced/ihp/pull/2214))
- NixOS: `sessionSecretFile` option and `appKeygen` service
- Migrations made optional in deployment configuration

### Performance

- Precompute static HSX subtrees at compile time ([#2248](https://github.com/digitallyinduced/ihp/pull/2248))
- `ByteString.Builder` in `toSQL'` to reduce allocations ([#2244](https://github.com/digitallyinduced/ihp/pull/2244))
- ByteString parsing for route params — no Text roundtrip ([#2241](https://github.com/digitallyinduced/ihp/pull/2241))
- `Text` instead of `String` in `pathTo` ([#2226](https://github.com/digitallyinduced/ihp/pull/2226))
- Serve `/static/` requests without middleware ([#2237](https://github.com/digitallyinduced/ihp/pull/2237))
- Skip request body parsing for GET/HEAD/DELETE/OPTIONS requests
- Eliminate double conversion in `respondHtml`
- Split `Generated.ActualTypes` into per-table modules for parallel compilation ([#2254](https://github.com/digitallyinduced/ihp/pull/2254))
- Eliminate `callCabal2nix` IFD for faster Nix evaluation ([#2251](https://github.com/digitallyinduced/ihp/pull/2251), [#2253](https://github.com/digitallyinduced/ihp/pull/2253))
- Replace ClassyPrelude with lightweight specific imports (57% faster ControllerSupport compilation) ([#2241](https://github.com/digitallyinduced/ihp/pull/2241))
- Split `IHP.ModelSupport`, `IHP.FrameworkConfig`, `IHP.HaskellSupport`, `IHP.CSSFramework` into sub-modules for faster compilation
- Refactor `QueryBuilder` into sub-modules for faster compilation
- Add `IHP.IDE.Prelude` for faster IDE controller compilation
- Optimize `IHP.Breadcrumb.Types` compilation by removing heavy imports
- Disable profiling and haddock for faster local builds ([#2206](https://github.com/digitallyinduced/ihp/pull/2206))
- Disable Haddock for generated models package
- Use `pg_isready` for faster Postgres startup polling ([#2208](https://github.com/digitallyinduced/ihp/pull/2208))
- Improve HSX parser performance and code generation
- Hoist `actionPrefixText` computation out of per-constructor loop
- Break serial compilation dependencies for better Nix parallelism ([#2252](https://github.com/digitallyinduced/ihp/pull/2252))
- Build jobs and web server concurrently in `nix build`

### Package Extractions

The following modules have been extracted into standalone packages:

| Module | New Package |
|--------|-------------|
| `IHP.Mail` | `ihp-mail` |
| `IHP.SchemaCompiler` | `ihp-schema-compiler` |
| `IHP.IDE.SchemaDesigner.Parser` | `ihp-postgres-parser` |
| `IHP.Controller.Param` (request params) | `wai-request-params` |
| `IHP.FlashMessages.*` | `wai-flash-messages` |
| `IHP.Assets` | `wai-asset-path` |
| `IHP.FileStorage.Preprocessor.ImageMagick` | `ihp-imagemagick` |
| `IHP.Job.Dashboard.*` | `ihp-job-dashboard` |
| `IHP.DataSync.*` | `ihp-datasync` |
| `IHP.Welcome.Controller` | `ihp-welcome` |
| `IHP.ControllerContext` | `ihp-context` |
| `IHP.PageHead.*` | `ihp-pagehead` |
| `IHP.Log.*` | `ihp-log` |
| `IHP.Modal.*` | `ihp-modal` |

All extracted modules are still re-exported from `ihp` for backwards compatibility.

### Frontend Dependency Upgrades

- jQuery 3.6.0 → 4.0.0 (with 3.7.1 as intermediate step)
- Bootstrap 5.2.1 → 5.3.8
- Select2 4.0.13 → 4.1.0-rc.0
- Old jQuery versions kept in `ihp-static` for backwards compatibility
- Lodash (DataSync) 4.17.21 → 4.17.23

### Bug Fixes

- Fix flash messages by using current request in `renderHtml` ([#2232](https://github.com/digitallyinduced/ihp/pull/2232))
- Fix `<<loop>>` error caused by lazy evaluation of TMap after `putContext`
- Fix context freezing prematurely in render function
- Fix HTML5 required validation for date and datetime fields ([#2182](https://github.com/digitallyinduced/ihp/pull/2182))
- Fix `ConversionFailed` error for tables with generated columns ([#2179](https://github.com/digitallyinduced/ihp/pull/2179))
- Fix WebSocket `onPing` not called ([#2138](https://github.com/digitallyinduced/ihp/pull/2138))
- Fix DevServer stdout/stderr buffering ([#2137](https://github.com/digitallyinduced/ihp/pull/2137))
- Fix `run-script` not working / multiple loads of the application ([#2141](https://github.com/digitallyinduced/ihp/pull/2141))
- Fix deploy script migrate service check
- Fix incomplete pattern matches in MigrationGenerator for PostgreSQL 17 compatibility
- Fix missing `NoRenderCommentNode` pattern in `nodeOuterHtml`
- Fix type equality operator warning by re-exporting `(~)` from Prelude
- Fix `hsDataDir` picking doc directory on x86_64-linux
- Fix `make db` failing
- Fix crash in ToolServer
- Fix missing `UrlInput` pattern in Bootstrap CSS framework
- Use `modelContextMiddleware` to populate request vault in test helpers ([#2174](https://github.com/digitallyinduced/ihp/pull/2174))
- Drop `Default Bool` instance for data-default >= 0.8
- Use `Data.Text` qualified in `IHP.NameSupport` to fix build with text-2.1.2
- Fix `uriToString` instead of `show` for URI serialization
- Fix duplicate Cabal modules from nested Generated subdirectories

### DataSync JavaScript Fixes

- Fix `slice()` → `splice()` so optimistic record IDs are actually removed ([#2263](https://github.com/digitallyinduced/ihp/pull/2263))
- Guard `splice()` against `indexOf` returning -1
- Handle null/undefined value in `whereTextSearchStartsWith`
- Fix UUID fallback variant bits by parsing hex digit properly
- Use strict equality for `OpEqual`/`OpNotEqual` in `recordMatchesQuery`
- Await recursive `close()` so callers wait for actual cleanup
- Initialize headers in `fetchAuthenticated` if missing
- Add error handling to `useCount`'s `sendMessage` call
- Retry reconnection after failure instead of giving up
- Log error when reconnect fails instead of silently swallowing it
- Omit empty `changeSet`/`appendSet` from `DidUpdate` JSON
- Fix memory leaks when connection is overloaded

### Developer Experience

- Improved HSX parser error messages with suggestions ([#2238](https://github.com/digitallyinduced/ihp/pull/2238))
- Full middleware stack in test requests ([#2228](https://github.com/digitallyinduced/ihp/pull/2228))
- Updated documentation for Bootstrap 5 ([#2193](https://github.com/digitallyinduced/ihp/pull/2193))
- Updated deployment guide to match modular config structure ([#2257](https://github.com/digitallyinduced/ihp/pull/2257))
- Simplified installation guide for Determinate Nix
- `redirect` now uses 303 See Other for auth redirects ([#2199](https://github.com/digitallyinduced/ihp/pull/2199))
- Set line buffering for stdout and stderr in DevServer
- Use default GHC from nixpkgs instead of pinned version

### DevEnv Updates

- devenv v1.8.2 → v1.10 → v1.11.2
- Use devenv postgres instead of IHP's built-in postgres
- Add binary cache to flake
- Improve caching of nix builds

### Internal / Refactoring

- Refactor generated types to build as proper Haskell package
- Split `IHP.ModelSupport` into Types module
- Split `IHP.FrameworkConfig` into Types module
- Extract `IHP.InputValue` from `IHP.ModelSupport`
- Extract `IHP.Record` from `IHP.HaskellSupport`
- Extract `HasPath` class to `IHP.Router.UrlGenerator`
- Extract middleware stack into `initMiddlewareStack` factory function
- Remove unused `IHP.GenericController` and empty `IHP.ViewErrorMessages` module
- Remove unused `viewContext` alias from ViewSupport
- Remove `fsnotify` override (nixpkgs now has 0.4.4.0)
- Remove unnecessary HLS fix for GHC 9.10
- Remove `quasiquoter` from `IHP.EnvVar`
- Replace deprecated `runCommandNoCC` with `runCommand` in Nix expressions
- Move `IHP.SchemaMigration` to `ihp-migrate` package
- Move `IHP.Test.Database` to `ihp-hspec` package ([#2163](https://github.com/digitallyinduced/ihp/pull/2163))
- Extract `PrimaryKey` instances into `Generated.ActualTypes.PrimaryKeys` module
- Add `relationSupport` Nix option for declarative configuration
