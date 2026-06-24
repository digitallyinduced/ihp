# IHP Changelog

## Unreleased

- The schema compiler now also generates a `DefaultParamEncoder [Maybe <Enum>]` instance for each enum type, alongside the existing `<Enum>`, `Maybe <Enum>`, and `[<Enum>]` instances. This lets `${[Just enumVal]}` (a list of nullable enum values) be interpolated into `typedSql` queries, e.g. `WHERE status = ANY(${[Just Active, Just Pending]})`, removing the need to split enum-filtered joins into fetch-ids-then-`filterWhereIn` or to cast through text in SQL.
- Added an `ihp-typed-sql` runtime test that round-trips an enum, a `Maybe` enum (including `Nothing` as SQL `NULL`), and a `[Maybe Enum]` list through `${...}` interpolation in a `WHERE` clause.

## v1.6.0 (2026-06-20)

### Breaking Changes

- Controller `action` now returns `IO ResponseReceived` instead of `IO ()`. `render`, `redirectTo`, `renderJson`, and related response helpers return the WAI response token directly instead of throwing `ResponseException`; use `earlyReturn` / `respondAndExit` for conditional exits. `ResponseException`, `handleNoResponseReturned`, and `handleRouterException` were removed from the public error-handling path. ([#2205](https://github.com/digitallyinduced/ihp/pull/2205))
- `render` now renders HTML only. JSON rendering moved to the new `JsonView` class and `renderHtmlOrJson`; existing views that implemented `json` on `View` need a separate `JsonView` instance. ([#2589](https://github.com/digitallyinduced/ihp/pull/2589))
- Authentication moved out of `initContext` and into WAI middleware. `initAuthentication` and the old role helper family were removed; configure `AuthMiddleware (authMiddleware @User)` in `Config.hs` instead. Auth state now lives in the request vault, and `FrameworkConfig.authMiddleware` was renamed to `authenticationMiddleware`. ([#2259](https://github.com/digitallyinduced/ihp/pull/2259), [#2641](https://github.com/digitallyinduced/ihp/pull/2641))
- The `ControllerContext` typed-map API was removed. `putContext`, `fromContext`, `freeze`, `unfreeze`, `FrozenControllerContext`, the `IHP.Controller.Context` module, and the `ihp-context` package are gone; use the WAI request vault for per-request state. `ControllerContext` is now a `Request` alias and is no longer re-exported from `IHP.ViewPrelude`. ([#2632](https://github.com/digitallyinduced/ihp/pull/2632), [#2633](https://github.com/digitallyinduced/ihp/pull/2633), [#2711](https://github.com/digitallyinduced/ihp/pull/2711))
- QueryBuilder join support was removed: `innerJoin`, `innerJoinThirdTable`, `labelResults`, and the joined-table filter/order helpers are gone. Use `typedSql` for joins and custom SQL. ([#2599](https://github.com/digitallyinduced/ihp/pull/2599))
- The `ihp-log` package and `IHP.Log.*` modules were removed. Use `fast-logger` directly through `?context.frameworkConfig.logger`, `?modelContext.logger`, or `FrameworkConfig.logger`. ([#2600](https://github.com/digitallyinduced/ihp/pull/2600))
- `typedSql` is stricter: `SELECT *` and `INSERT` without explicit column lists are rejected by default, and `int8` / `bigint` values now map to `Int64` instead of `Integer`. QueryBuilder `limit` and `offset` also use `Int64` to match PostgreSQL. ([#2621](https://github.com/digitallyinduced/ihp/pull/2621), [#2655](https://github.com/digitallyinduced/ihp/pull/2655), [#2677](https://github.com/digitallyinduced/ihp/pull/2677))
- Production app packages no longer include binaries for `Application/Script/*.hs`. Scripts are exposed as separate `script-Name` flake outputs and apps. ([#2715](https://github.com/digitallyinduced/ihp/pull/2715))
- Dev mode now starts separate `web` and `worker` processes. Projects with jobs should move the root `Worker` instance from `Main.hs` into a new top-level `WorkerMain.hs`; `new-job` creates this file for new projects. The old `processes.ihp` devenv process name is now split into `processes.web` and `processes.worker`. ([#2672](https://github.com/digitallyinduced/ihp/pull/2672))
- `devenv up` no longer opens the ToolServer in a browser by default. The URL is printed to the terminal; set `IHP_BROWSER` to opt back in. ([#2713](https://github.com/digitallyinduced/ihp/pull/2713))
- Generated app development defaults now promote incomplete pattern matches to errors, and all IHP packages treat missing Cabal `other-modules` entries as errors. ([#2687](https://github.com/digitallyinduced/ihp/pull/2687), [#2692](https://github.com/digitallyinduced/ihp/pull/2692))
- Authentication sessions now store raw UUID bytes. `parseSessionUUID` accepts both the current raw UUID format and legacy 44-byte cereal-encoded UUID values for compatibility, but custom login/session code should write the raw format. ([#2640](https://github.com/digitallyinduced/ihp/pull/2640), [#2680](https://github.com/digitallyinduced/ihp/pull/2680))

### Deprecations

- The untyped raw-SQL helpers `sqlQuery`, `sqlQuerySingleRow`, `sqlExec`, `sqlExecDiscardResult`, `sqlQueryScalar`, and `sqlQueryScalarOrNothing` are now deprecated. Prefer the compile-time-checked `[typedSql| … |]` quasi-quoter with `sqlQueryTyped` / `sqlExecTyped` (from `IHP.TypedSql`). When you genuinely need untyped/dynamic SQL (dynamic table names, DDL), switch to the new `unsafeSql*` variants — identical behavior, no warning. ([#2651](https://github.com/digitallyinduced/ihp/pull/2651))

### New Features

- New explicit routes DSL via `[routes|...|]` for `Web/Routes.hs`, including RFC 6570 path captures, explicit query parameters, multi-controller route blocks, WebSocket routes, GET-to-HEAD handling, and 400/405 responses for invalid methods. New controllers are scaffolded with the DSL by default while AutoRoute remains supported. ([#2652](https://github.com/digitallyinduced/ihp/pull/2652), [#2663](https://github.com/digitallyinduced/ihp/pull/2663), [#2678](https://github.com/digitallyinduced/ihp/pull/2678), [#2679](https://github.com/digitallyinduced/ihp/pull/2679))
- New standalone `ihp-router` package containing the trie router, WAI middleware, URL capture machinery, and `[routes|...|]` quasi-quoter without depending on the rest of IHP. IHP re-exports the IHP-specific router integration through `IHP.Router.IHP`. ([#2657](https://github.com/digitallyinduced/ihp/pull/2657), [#2658](https://github.com/digitallyinduced/ihp/pull/2658), [#2659](https://github.com/digitallyinduced/ihp/pull/2659), [#2660](https://github.com/digitallyinduced/ihp/pull/2660))
- `typedSql` can start a temporary private PostgreSQL instance during compile-time query description when `DATABASE_URL` is unavailable and `IHP_TYPED_SQL_AUTO_DB=1` is set; IHP dev shells enable this by default. ([#2587](https://github.com/digitallyinduced/ihp/pull/2587), [#2714](https://github.com/digitallyinduced/ihp/pull/2714))
- `typedSql` multi-column results now generate named `SqlRow` types for record-dot field access, infer more non-null expressions (`COUNT`, `EXISTS`, window functions, non-null literals), and report polymorphic placeholder type errors with better messages. ([#2591](https://github.com/digitallyinduced/ihp/pull/2591), [#2596](https://github.com/digitallyinduced/ihp/pull/2596), [#2669](https://github.com/digitallyinduced/ihp/pull/2669))
- Added `fetchVector` and `fetchVectorPipelined` for returning `Vector` results from QueryBuilder queries. ([#2592](https://github.com/digitallyinduced/ihp/pull/2592))
- Added `buildMail` to the public `IHP.Mail` exports. ([#2586](https://github.com/digitallyinduced/ihp/pull/2586))
- Added `uncheckedHsx` and `customHsx` to the controller/view preludes, and added `isEmpty` for `Html` / `MarkupM`. ([#2594](https://github.com/digitallyinduced/ihp/pull/2594), [#2619](https://github.com/digitallyinduced/ihp/pull/2619))
- Schema parsing and codegen now understand more PostgreSQL syntax: `ANY(ARRAY[...])` in checks, `NULLS [NOT] DISTINCT` indexes, pgvector columns/indexes, VARIADIC function arguments in pg_dump indexes, function `SET` options, and chained postfix operators such as qualified columns with `IN`, casts, and field access. ([#2614](https://github.com/digitallyinduced/ihp/pull/2614), [#2635](https://github.com/digitallyinduced/ihp/pull/2635), [#2673](https://github.com/digitallyinduced/ihp/pull/2673), [#2681](https://github.com/digitallyinduced/ihp/pull/2681), [#2682](https://github.com/digitallyinduced/ihp/pull/2682), [#2683](https://github.com/digitallyinduced/ihp/pull/2683))
- Added an `IsScalar Integer` instance so `BIGSERIAL` / `BIGINT` primary keys compile, and pooled PostgreSQL connections now pin the session timezone to UTC. ([#2647](https://github.com/digitallyinduced/ihp/pull/2647), [#2648](https://github.com/digitallyinduced/ihp/pull/2648))
- DataSync's JavaScript client is now authored in TypeScript with generated table registries, typed event maps, and narrower generic public APIs. ([#2578](https://github.com/digitallyinduced/ihp/pull/2578))
- IHP projects can put local Haskell libraries under `lib`, and the `ihp.ghcCompiler` option is now wired through so projects can opt into GHC 9.14 tooling. ([#2694](https://github.com/digitallyinduced/ihp/pull/2694), [#2705](https://github.com/digitallyinduced/ihp/pull/2705), [#2710](https://github.com/digitallyinduced/ihp/pull/2710))
- The dev server honors the `PORT` environment variable. ([#2717](https://github.com/digitallyinduced/ihp/pull/2717))
- Apache request logs can default `%u` to the current user's UUID. ([#2649](https://github.com/digitallyinduced/ihp/pull/2649))

### Performance, Build, and Tooling

- HSX rendering now uses a direct `ByteString.Builder` markup backend instead of the Blaze `MarkupM` tree, giving 2-4x faster HSX rendering in benchmarks. `respondHtml` / `respondSvg` use WAI builders directly for zero-copy responses. ([#2563](https://github.com/digitallyinduced/ihp/pull/2563))
- `pathTo` and `renderFieldForUrl` are marked `NOINLINE` to reduce Core bloat, and the new routes DSL caches the merged route trie at application construction time. ([#2524](https://github.com/digitallyinduced/ihp/pull/2524), [#2652](https://github.com/digitallyinduced/ihp/pull/2652))
- Nix production builds now compile web, worker, and script executables in separate derivations, allowing more parallelism and better cache reuse. ([#2715](https://github.com/digitallyinduced/ihp/pull/2715))
- Nix evaluation no longer relies on eval-time import-from-derivation for IHP data paths or Hackage package metadata; Hackage overrides are pre-generated. ([#2612](https://github.com/digitallyinduced/ihp/pull/2612), [#2620](https://github.com/digitallyinduced/ihp/pull/2620))
- The core `ihp` library closure is smaller after dropping unused `lens` / `wreq` dependencies and moving `IHP.Test.Mocking` so normal apps do not pull in `hspec`. ([#2626](https://github.com/digitallyinduced/ihp/pull/2626), [#2627](https://github.com/digitallyinduced/ihp/pull/2627))
- Dev-server GHCi now uses the copying GC so memory is returned to the OS more aggressively, and job-worker shutdown drains in-flight work more reliably. ([#2712](https://github.com/digitallyinduced/ihp/pull/2712), [#2721](https://github.com/digitallyinduced/ihp/pull/2721))
- CI now uses Magic Nix Cache and the self-hosted digitally induced binary cache, and GitHub-hosted runners avoid multi-GHC OOM failures. ([#2605](https://github.com/digitallyinduced/ihp/pull/2605), [#2608](https://github.com/digitallyinduced/ihp/pull/2608), [#2609](https://github.com/digitallyinduced/ihp/pull/2609), [#2700](https://github.com/digitallyinduced/ihp/pull/2700), [#2703](https://github.com/digitallyinduced/ihp/pull/2703), [#2704](https://github.com/digitallyinduced/ihp/pull/2704))
- The Guide layout was redesigned and the Guide/API-doc links were cleaned up, including generated API documentation links for extracted subpackages. ([#2666](https://github.com/digitallyinduced/ihp/pull/2666), [#2697](https://github.com/digitallyinduced/ihp/pull/2697), [#2716](https://github.com/digitallyinduced/ihp/pull/2716))

### Bug Fixes

- Fixed generated `createMany` statements so columns with defaults, including `id`, use `DEFAULT` correctly and mixed touched-field sets are handled per row. ([#2585](https://github.com/digitallyinduced/ihp/pull/2585))
- Fixed DataSync concurrent trigger installation by using the correct advisory lock key. ([#2595](https://github.com/digitallyinduced/ihp/pull/2595))
- Fixed `filterWhere` / generated field-name handling for keyword-escaped fields ending in an underscore. ([#2602](https://github.com/digitallyinduced/ihp/pull/2602))
- Fixed WebSocket upgrades being logged as 500 instead of 101, fixed AutoRefresh WebSocket fallback behavior, and restored the AutoRefresh meta tag after the request-vault migration. ([#2625](https://github.com/digitallyinduced/ihp/pull/2625), [#2631](https://github.com/digitallyinduced/ihp/pull/2631), [#2637](https://github.com/digitallyinduced/ihp/pull/2637))
- Fixed `withUser` / `authMiddleware` tests preserving mocked sessions across `sessionMiddleware`. ([#2639](https://github.com/digitallyinduced/ihp/pull/2639))
- AJAX/fetch requests now get JSON error responses from the error middleware. ([#2634](https://github.com/digitallyinduced/ihp/pull/2634))
- Restored `ConvertibleStrings` `Text`/`String` `Html` instances for compatibility. ([#2638](https://github.com/digitallyinduced/ihp/pull/2638))
- Fixed `renderFilter` accumulating duplicate query parameters. ([#2719](https://github.com/digitallyinduced/ihp/pull/2719))
- Fixed router exception wrapping so router exceptions are handled by the framework error path. ([#2718](https://github.com/digitallyinduced/ihp/pull/2718))
- Missing prepared statements are retried after PostgreSQL invalidates a prepared statement cache entry. ([#2724](https://github.com/digitallyinduced/ihp/pull/2724))
- Migration tooling now shows actual migration failure messages and fails on duplicate migration timestamps. ([#2617](https://github.com/digitallyinduced/ihp/pull/2617), [#2723](https://github.com/digitallyinduced/ihp/pull/2723))
- Fixed orphaned GHCi processes on Ctrl+C / SIGTERM, and surfaced app startup crashes on the dev-server error page. ([#2603](https://github.com/digitallyinduced/ihp/pull/2603), [#2607](https://github.com/digitallyinduced/ihp/pull/2607), [#2722](https://github.com/digitallyinduced/ihp/pull/2722))

## v1.5.0 (2026-03-25)

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
- Session packages replaced: `wai-session` → `wai-session-maybe` (`Network.Wai.Session` → `Network.Wai.Session.Maybe`), `wai-session-clientsession` → `wai-session-clientsession-deferred` (`Network.Wai.Session.ClientSession` → `Network.Wai.Session.ClientSession.Deferred`) ([#2582](https://github.com/digitallyinduced/ihp/pull/2582))

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
- Separate `nixpkgs-nixos` flake input for pinning NixOS deployment nixpkgs independently from Haskell package nixpkgs ([#2519](https://github.com/digitallyinduced/ihp/pull/2519))

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
- Lazy session middleware: skip decrypt/encrypt when session is unused ([#2328](https://github.com/digitallyinduced/ihp/pull/2328), [#2582](https://github.com/digitallyinduced/ihp/pull/2582)) — published as [`wai-session-maybe`](https://hackage.haskell.org/package/wai-session-maybe) and [`wai-session-clientsession-deferred`](https://hackage.haskell.org/package/wai-session-clientsession-deferred) on Hackage (2.9-3.1x throughput improvement for routes that don't access the session)
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
| Session middleware (`wai-session` fork) | [`wai-session-maybe`](https://hackage.haskell.org/package/wai-session-maybe) |
| Session clientsession (`wai-session-clientsession` fork) | [`wai-session-clientsession-deferred`](https://hackage.haskell.org/package/wai-session-clientsession-deferred) |

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
- Fix DataSync trigger installation causing AccessExclusiveLock on every server restart ([#2580](https://github.com/digitallyinduced/ihp/pull/2580))
- Fix DataSubscription crash during WebSocket reconnect ([#2576](https://github.com/digitallyinduced/ihp/pull/2576))
- Fix Flatpickr shifting datetime values by user's timezone offset on every edit ([#2570](https://github.com/digitallyinduced/ihp/pull/2570))
- Fix Hoogle not loading in Safari due to CSP headers on localhost ([#2567](https://github.com/digitallyinduced/ihp/pull/2567))
- Fix Cabal 3.12+ aborting configure due to duplicate GHC2021 in default-extensions ([#2572](https://github.com/digitallyinduced/ihp/pull/2572))
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
- Comprehensive documentation improvements for beginners: security, flash messages, JSON API, production checklist, and 10+ other guides ([#2577](https://github.com/digitallyinduced/ihp/pull/2577))
- Passkeys (WebAuthn) authentication guide ([#2574](https://github.com/digitallyinduced/ihp/pull/2574))

### Job Queue

- Job worker redesign for reliability and performance ([#2327](https://github.com/digitallyinduced/ihp/pull/2327))
- Dev-only job poller trigger self-heal ([#2470](https://github.com/digitallyinduced/ihp/pull/2470))
- Decouple job queue from ModelContext by passing HasqlPool explicitly
- IHP.Job refactored into focused submodules ([#2471](https://github.com/digitallyinduced/ihp/pull/2471))

### DevEnv Updates

- devenv v1.8.2 → v1.10 → v1.11.2 → v2.0.2 → v2.0.6 ([#2475](https://github.com/digitallyinduced/ihp/pull/2475), [#2568](https://github.com/digitallyinduced/ihp/pull/2568))
- Switch devenv to process-compose process manager ([#2566](https://github.com/digitallyinduced/ihp/pull/2566))
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
- Use Hackage versions of hasql-mapping, hasql-postgresql-types, postgresql-simple-postgresql-types
- Split `IHP.View.Form` into sub-modules ([#2550](https://github.com/digitallyinduced/ihp/pull/2550))
- Extract `ihp-pglistener` package ([#2273](https://github.com/digitallyinduced/ihp/pull/2273))
- Simplify hasql pool retry using upstream auto-discard
- `CSSFramework` split into separate modules, removed ~295 lines of duplicated render code
- GHC 9.12 support (experimental, opt-in)
- GHC 9.14 compatibility checks added ([#2515](https://github.com/digitallyinduced/ihp/pull/2515))
- NamedDefaults for IsString (GHC 9.12+, CPP-gated) ([#2530](https://github.com/digitallyinduced/ihp/pull/2530))
