# Changelog for `ihp-ide`

## v1.6.0

- Split dev mode into separate web and worker processes
- Add `WorkerMain.hs` scaffolding for job workers
- Print the ToolServer URL instead of opening a browser automatically
- Honor the `PORT` environment variable in the dev server
- Surface app startup crashes on the dev-server error page
- Switch dev-server GHCi to the copying GC so memory is returned to the OS more
  aggressively
- Fix job worker shutdown draining
- Fix orphaned GHCi processes on Ctrl+C / SIGTERM
- Show the actual error message when IDE migrations fail
- Scaffold new controllers with the explicit routes DSL by default
- Remove the IDE sidebar help button and StackOverflow links

## v1.5.1

- Restore missing test sources in sdist: `Test.IDE.ToolServer.MiddlewareSpec` and `Test.IDE.Logs.ControllerSpec` were imported by `Test/Main.hs` but not declared in the cabal `test-suite > other-modules`, so they were excluded from the Hackage tarball and broke downstream builds (e.g. nixpkgs). No source changes — manifest fix only.

## v1.5.0

- Add PostgreSQL table inheritance (`INHERITS`) support in Schema Designer
- Add PostgreSQL 18 support with UUIDv7 as default UUID function
- Support multiple trigger events (INSERT OR UPDATE, etc.)
- Add `SECURITY DEFINER` support for SQL functions
- Enable Hoogle by default with auto-start and IDE sidebar link
- Allow deselecting actions in the controller generator
- Add app selector to Generate Controller from schema designer
- Improve IDE logs viewer with devenv service log tabs
- Add persistent socket for seamless app restarts
- Migrate IDE Data Controller from postgresql-simple to hasql
- Migrate Bootstrap 4 to Bootstrap 5 (CSS classes, data attributes, jQuery 4.0)
- Upgrade Bootstrap 5.2.1 to 5.3.8
- Fix Schema Designer layout issues, column visibility, and context menu width
- Fix IDE Data Editor foreign key dropdown, tooltip positioning, boolean display
- Fix app crash detection with persistent socket
- Fix lazy IO crash in FileWatcher
- Fix devenv up Ctrl+C leaving GHCi/web server running as orphan processes
- Remove compile-time `+RTS` flags (were incompatible with Hackage)
- Replace snippet codegen with hasql statement modules
- Improve code generators with type-aware scaffolding
- Use `OsPath` instead of `FilePath`
