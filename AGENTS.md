This file provides guidance to AI when working with code in this repository.

## Overview

IHP (Integrated Haskell Platform) is a batteries-included Haskell web framework built on Haskell and Nix. It follows an MVC architecture with type-safe routing, HSX templating (JSX-like syntax for HTML), and PostgreSQL as the database.

## Development Environment Setup

IHP uses devenv.sh (a wrapper around `nix develop` and `direnv`) for development:

```bash
cd /path/to/ihp
direnv allow
```

## Running Tests

**IHP IDE Tests** (from the repo root):
```bash
# Interactive (allows :r to reload after changes):
ghci
:l ihp-ide/Test/Main.hs
main
:r
main

# One-liner:
echo -e ':l ihp-ide/Test/Main.hs\nmain' | ghci
```

**All tests via Nix** (slower but comprehensive):
```bash
nix flake check --impure
```

**Verify a single module compiles:**
```bash
echo ':l ihp-ide/IHP/IDE/CodeGen/MigrationGenerator.hs' | ghci
```

When adding new tests, add them to `Test/Main.hs` in the appropriate package.

## Running the Dev Server (for local IHP development)

From the IHP directory with a host project:
```bash
export DEBUG=1  # Optional: enable debug logging
ghci
:l ihp-ide/exe/IHP/IDE/DevServer.hs
mainInParentDirectory
```

## Project Structure

This is a monorepo with multiple Haskell packages:

| Package | Purpose |
|---------|---------|
| `ihp/` | Core web framework (routing, controllers, views, models, validation) |
| `ihp-ide/` | Development server, code generators, schema designer |
| `ihp-hsx/` | HSX templating system (JSX-like HTML in Haskell) |
| `ihp-migrate/` | Database migration tool |
| `ihp-datasync/` | Real-time data synchronization via WebSockets |
| `ihp-graphql/` | GraphQL API support |
| `ihp-hspec/` | Testing utilities |
| `ihp-openai/` | OpenAI integration |
| `ihp-ssc/` | Server-side components |
| `ihp-postgresql-simple-extra/` | PostgreSQL utilities |

## Core Framework Architecture (`ihp/IHP/`)

- **Prelude modules**: `Prelude.hs`, `ControllerPrelude.hs`, `ViewPrelude.hs`, `RouterPrelude.hs`, `MailPrelude.hs` - standard imports for different contexts
- **Server.hs**: HTTP server initialization and middleware
- **FrameworkConfig.hs**: Framework configuration
- **RouterSupport.hs**: Type-safe routing and URL generation
- **QueryBuilder.hs**: Type-safe SQL query builder
- **ModelSupport.hs**: ORM-like database model support
- **Controller/**: Request handling, params, sessions, cookies, redirects
- **View/**: Form helpers, CSS framework integration, rendering
- **ValidationSupport/**: Field validation
- **AuthSupport/**: Authentication
- **LoginSupport/**: Login functionality
- **Job/**: Background job queue and runner
- **FileStorage/**: File upload abstraction
- **AutoRefresh.hs**: Real-time view updates via WebSocket
- **Mail.hs**: Email composition and sending

## IDE/Code Generation (`ihp-ide/`)

- **exe/IHP/IDE/DevServer.hs**: Development server entry point
- **IHP/IDE/SchemaDesigner/**: Visual database schema editor
- **IHP/IDE/CodeGen/**: Code generators for controllers, views, migrations, jobs, etc.
- **IHP/SchemaCompiler.hs**: Generates `Types.hs` from SQL schema

## Code Guidelines

- Use `pure` instead of `return` (clearer for non-Haskell developers)
- Add Haddock comments to public APIs
- The framework uses implicit parameters extensively (see `ImplicitParams` extension)
- HSX uses quasiquotes: `[hsx|<div>content</div>|]`

## Key Language Extensions

The codebase uses GHC2021 with these notable extensions:
- `ImplicitParams`: For passing context implicitly
- `OverloadedLabels`: For field access (`#fieldName`)
- `QuasiQuotes`: For HSX templates
- `TypeFamilies` and `DataKinds`: For type-safe routing and models

## Faster Development Builds

For faster rebuilds during development, uncomment the `configureFlags = [ "--flag FastBuild" ];` line in `ihp.nix`. Don't commit this change:
```bash
git update-index --assume-unchanged ihp.nix
```

## Working with a Local IHP in an App

Clone IHP into your app directory, then patch `flake.nix`:
```bash
git clone git@github.com:digitallyinduced/ihp.git IHP
cd IHP
sed -i "s|ihp.url = .*|ihp.url = \"path://$(pwd)\";|" ../flake.nix
direnv allow
cd ..
nix flake update
```

## Documentation

The `/Guide/` directory contains comprehensive documentation in markdown. Run `devenv up` from the Guide directory to preview changes locally.
