# IHP Changelog

## v1.5.0 (Unreleased)

1,051 commits since v1.4.0. 607 files changed, 46,204 insertions, 27,040 deletions.

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
- `requestBodyJSON` now returns `IO Aeson.Value` instead of `Aeson.Value`; update call sites to bind it in `do`-notation ([#2396](https://github.com/digitallyinduced/ihp/pull/2396))
- Migrated from `postgresql-simple` to `hasql` for all database access — applications using `Database.PostgreSQL.Simple` directly need to migrate to IHP's query builder or `typedSql`
- `touchedFields` changed from `[Text]` to `Integer` bitmask for better performance ([#2473](https://github.com/digitallyinduced/ihp/pull/2473))
- `CSSFramework` `Default` instance removed — use `unstyled` instead of `def`; new `styledLabelClass` field added to the record
- `requestBodyJSON` now returns HTTP 400 for malformed JSON instead of crashing

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
- Typed SQL (`ihp-typed-sql` package) — type-safe SQL queries with automatic decoder inference and JOIN nullability detection ([#2304](https://github.com/digitallyinduced/ihp/pull/2304))
- `fetchPipelined` for batching multiple database queries in a single round-trip using PostgreSQL pipeline mode ([#2459](https://github.com/digitallyinduced/ihp/pull/2459))
- Composite primary key support — tables with composite PKs now generate correct types and HasField instances ([#992](https://github.com/digitallyinduced/ihp/pull/992), [#2553](https://github.com/digitallyinduced/ihp/pull/2553))
- PostgreSQL table inheritance (INHERITS) support in schema designer ([#2505](https://github.com/digitallyinduced/ihp/pull/2505))
- SECURITY DEFINER support for SQL functions ([#2504](https://github.com/digitallyinduced/ihp/pull/2504))
- Multiple trigger events support (e.g. INSERT OR UPDATE) ([#2508](https://github.com/digitallyinduced/ihp/pull/2508))
- Integration test support with automatic temporary PostgreSQL database ([#2510](https://github.com/digitallyinduced/ihp/pull/2510))
- `withIHPApp` public API for testing ([#2314](https://github.com/digitallyinduced/ihp/pull/2314))
- `runDevScript` for easy GHCi script execution ([#2539](https://github.com/digitallyinduced/ihp/pull/2539))
- `devHaskellPackages` option for dev-only Haskell dependencies ([#2529](https://github.com/digitallyinduced/ihp/pull/2529))
- Docker worker images for background job runners ([#2541](https://github.com/digitallyinduced/ihp/pull/2541))
- IDE logs viewer with devenv service tabs ([#2499](https://github.com/digitallyinduced/ihp/pull/2499))
- App selector in Generate Controller preview from schema designer ([#2509](https://github.com/digitallyinduced/ihp/pull/2509))
- Deselectable actions in controller generator ([#2511](https://github.com/digitallyinduced/ihp/pull/2511))
- HSX wildcard pattern support ([#1852](https://github.com/digitallyinduced/ihp/pull/1852), [#2506](https://github.com/digitallyinduced/ihp/pull/2506))
- Type applications in HSX splices ([#2321](https://github.com/digitallyinduced/ihp/pull/2321))
- Hoogle documentation enabled by default ([#2512](https://github.com/digitallyinduced/ihp/pull/2512))
- Allow overriding nixpkgs config ([#2495](https://github.com/digitallyinduced/ihp/pull/2495))

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
- Optimize AutoRoute URL generation (`pathTo`) by 2.3x ([#2335](https://github.com/digitallyinduced/ihp/pull/2335))
- HashMap-based dispatch for AutoRoute routing
- Optimize per-request latency: flash messages + Accept header ([#2329](https://github.com/digitallyinduced/ihp/pull/2329))
- Lazy session middleware: skip decrypt/encrypt when session is unused ([#2328](https://github.com/digitallyinduced/ihp/pull/2328))
- INLINE pragmas on render hot-path functions ([#2333](https://github.com/digitallyinduced/ihp/pull/2333))
- Drop INLINE pragmas from fetch/query compilation chain to reduce Core bloat ([#2522](https://github.com/digitallyinduced/ihp/pull/2522))
- Use record update syntax for SetField/UpdateField codegen ([#2476](https://github.com/digitallyinduced/ihp/pull/2476))
- Replace Snippet with direct Hasql.Statement compilation ([#2435](https://github.com/digitallyinduced/ihp/pull/2435))
- Reduce GHCi dev server memory from ~4GB to ~500-800MB ([#2543](https://github.com/digitallyinduced/ihp/pull/2543))
- Mark `actionPrefixText` NOINLINE to reduce code bloat in views ([#2516](https://github.com/digitallyinduced/ihp/pull/2516))
- Tune GC settings for dev server to reduce request latency spikes ([#2323](https://github.com/digitallyinduced/ihp/pull/2323))

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
| `IHP.PGListener` | `ihp-pglistener` |

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
- Fix AutoRefresh and DataSync breaking after `make db` recreates the database ([#2295](https://github.com/digitallyinduced/ihp/pull/2295))
- Fix crash in ToolServer
- Fix missing `UrlInput` pattern in Bootstrap CSS framework
- Use `modelContextMiddleware` to populate request vault in test helpers ([#2174](https://github.com/digitallyinduced/ihp/pull/2174))
- Drop `Default Bool` instance for data-default >= 0.8
- Use `Data.Text` qualified in `IHP.NameSupport` to fix build with text-2.1.2
- Fix `uriToString` instead of `show` for URI serialization
- Fix duplicate Cabal modules from nested Generated subdirectories
- Fix AutoRefresh losing layout after vault migration ([#2337](https://github.com/digitallyinduced/ihp/pull/2337))
- Fix AutoRefresh losing query parameters during re-render ([#2401](https://github.com/digitallyinduced/ihp/pull/2401))
- Fix stale AutoRefresh response comparisons ([#2546](https://github.com/digitallyinduced/ihp/pull/2546))
- AutoRefresh gracefully degrades without PGListener ([#2465](https://github.com/digitallyinduced/ihp/pull/2465))
- Fix DataSync append optimization double-applying ([#2544](https://github.com/digitallyinduced/ihp/pull/2544))
- Fix DataSync trigger installation deadlock ([#2468](https://github.com/digitallyinduced/ihp/pull/2468))
- Fix HasqlDecodeColumn Int using int8 instead of int4 ([#2493](https://github.com/digitallyinduced/ihp/pull/2493))
- Fix `filterWhere` with `Nothing` generating invalid `IS $N` instead of `IS NULL`
- Fix RowDecoder generating nullable decoder for PRIMARY KEY columns ([#2540](https://github.com/digitallyinduced/ihp/pull/2540))
- Fix controller generator producing invalid Id fields when table doesn't exist ([#1179](https://github.com/digitallyinduced/ihp/pull/1179), [#2559](https://github.com/digitallyinduced/ihp/pull/2559))
- Fix duplicate HasField "id" when table has composite PK and id column ([#989](https://github.com/digitallyinduced/ihp/pull/989), [#2557](https://github.com/digitallyinduced/ihp/pull/2557))
- Fix foreign key non-PK column type ([#2558](https://github.com/digitallyinduced/ihp/pull/2558))
- Fix modal close button not working ([#2561](https://github.com/digitallyinduced/ihp/pull/2561))
- Fix IDE Data Editor foreign key dropdown flickering ([#2489](https://github.com/digitallyinduced/ihp/pull/2489))
- Fix IDE toolbar help popover not opening ([#2494](https://github.com/digitallyinduced/ihp/pull/2494))
- Fix devenv up Ctrl+C leaving orphan processes ([#2527](https://github.com/digitallyinduced/ihp/pull/2527), [#2548](https://github.com/digitallyinduced/ihp/pull/2548))
- Fix devenv up Ctrl+C not stopping postgres ([#2485](https://github.com/digitallyinduced/ihp/pull/2485))
- Fix lazy IO crash in FileWatcher filterGitIgnored ([#2330](https://github.com/digitallyinduced/ihp/pull/2330))
- Suppress `-Wambiguous-fields` warnings in generated types ([#2487](https://github.com/digitallyinduced/ihp/pull/2487))
- Render hasql PostgreSQL errors with structured detail in dev mode ([#2399](https://github.com/digitallyinduced/ihp/pull/2399))
- Fix job worker silently exiting on transient fetchNextJob error ([#2453](https://github.com/digitallyinduced/ihp/pull/2453))

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
- i18n documentation ([#2503](https://github.com/digitallyinduced/ihp/pull/2503))
- Pagination guide ([#2551](https://github.com/digitallyinduced/ihp/pull/2551))
- Explain side-effect actions need forms, not links ([#2552](https://github.com/digitallyinduced/ihp/pull/2552))
- Docker migration-before-start pattern documentation ([#2555](https://github.com/digitallyinduced/ihp/pull/2555))

### Job Queue

- Job worker redesign for reliability and performance ([#2327](https://github.com/digitallyinduced/ihp/pull/2327))
- Dev-only job poller trigger self-heal ([#2470](https://github.com/digitallyinduced/ihp/pull/2470))
- Decouple job queue from ModelContext by passing HasqlPool explicitly
- IHP.Job refactored into focused submodules ([#2471](https://github.com/digitallyinduced/ihp/pull/2471))

### DevEnv Updates

- devenv v1.8.2 → v1.10 → v1.11.2 → v2.0.2 ([#2475](https://github.com/digitallyinduced/ihp/pull/2475))
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
- Full migration from `postgresql-simple` to `hasql` across all packages ([#2262](https://github.com/digitallyinduced/ihp/pull/2262), [#2311](https://github.com/digitallyinduced/ihp/pull/2311), [#2322](https://github.com/digitallyinduced/ihp/pull/2322), [#2326](https://github.com/digitallyinduced/ihp/pull/2326), [#2377](https://github.com/digitallyinduced/ihp/pull/2377))
- Hackage release preparation for Tier 0-4 packages ([#2533](https://github.com/digitallyinduced/ihp/pull/2533), [#2536](https://github.com/digitallyinduced/ihp/pull/2536), [#2537](https://github.com/digitallyinduced/ihp/pull/2537), [#2538](https://github.com/digitallyinduced/ihp/pull/2538))
- Use Hackage versions of hasql-mapping, hasql-postgresql-types, postgresql-simple-postgresql-types
- Split `IHP.View.Form` into sub-modules ([#2550](https://github.com/digitallyinduced/ihp/pull/2550))
- Extract `ihp-pglistener` package ([#2273](https://github.com/digitallyinduced/ihp/pull/2273))
- Simplify hasql pool retry using upstream auto-discard
- `CSSFramework` split into separate modules, removed ~295 lines of duplicated render code
- GHC 9.12 support (experimental, opt-in)
- GHC 9.14 compatibility checks added ([#2515](https://github.com/digitallyinduced/ihp/pull/2515))
- NamedDefaults for IsString (GHC 9.12+, CPP-gated) ([#2530](https://github.com/digitallyinduced/ihp/pull/2530))
