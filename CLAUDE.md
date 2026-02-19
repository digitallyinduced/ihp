# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

IHP (Integrated Haskell Platform) is a batteries-included Haskell web framework built on Haskell and Nix. It follows an MVC architecture with type-safe routing, HSX templating (JSX-like syntax for HTML), and PostgreSQL as the database.

## Development Environment Setup

IHP uses devenv.sh (a wrapper around `nix develop` and `direnv`) for development:

```bash
cd /path/to/ihp
direnv allow
```

Alternatively, use `nix develop` directly to enter a dev shell. You can also use `direnv exec .` to get a cached nix develop environment.

## Running Tests

Prefer `ghci` for quick type checking and iteration — full nix builds (`nix flake check`) are slow. Use ghci interactively or via `echo ... | ghci` one-liners for fast feedback.

**When using ghci, keep in mind that it expects input on stdin. If you don't provide any, ghci will never return.** The `echo ... | ghci` pattern handles this correctly by piping input.

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

## Hasql Database Patterns (Reference)

Based on patterns from [hasql-tutorial1](https://github.com/nikita-volkov/hasql-tutorial1). Hasql provides type-safe PostgreSQL access with a layered architecture.

### Module Structure

The tutorial demonstrates a library-oriented structure where database code is isolated into a reusable package:

```
my-db-library/
├── my-db-library.cabal
└── library/
    └── MyDbLibrary/
        ├── Prelude.hs      -- Re-exports (rerebase), internal
        ├── Statement.hs    -- Raw SQL statements, internal
        ├── Transaction.hs  -- Business logic composition, internal
        └── Session.hs      -- PUBLIC API (only exposed module)
```

**Cabal exposure pattern:**
```cabal
library
  exposed-modules:
    MyDbLibrary.Session           -- Only public module
  other-modules:
    MyDbLibrary.Prelude           -- Internal
    MyDbLibrary.Statement         -- Internal
    MyDbLibrary.Transaction       -- Internal
```

**Why this structure?**
- **Stable API**: Only `Session` is exposed, allowing internal refactoring without breaking consumers
- **Encapsulation**: Statements and transactions are implementation details
- **Reusability**: The library can be shared across multiple apps (CLI, REST API, etc.)
- **Testability**: Each layer can be tested independently

### Import Pattern

```haskell
-- In Statement.hs (internal)
module MyDbLibrary.Statement where

import MyDbLibrary.Prelude
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

-- In Transaction.hs (internal)
module MyDbLibrary.Transaction where

import MyDbLibrary.Prelude
import qualified MyDbLibrary.Statement as Statement
import qualified Hasql.Transaction as Transaction

-- In Session.hs (public API)
module MyDbLibrary.Session where

import MyDbLibrary.Prelude
import qualified MyDbLibrary.Transaction as Transaction
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Sessions
```

### Layer 1: Statements (Raw SQL + Codecs)

Statements combine SQL with type-safe encoders/decoders:

```haskell
import qualified Hasql.Statement as Hasql
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders

findUserByEmail :: Hasql.Statement Text (Maybe Int32)
findUserByEmail =
  Hasql.Statement sql encoder decoder True
  where
    sql = "SELECT id FROM users WHERE email = $1"
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.int4))

insertUser :: Hasql.Statement (Text, ByteString, Text, Maybe Text) Int32
insertUser =
  Hasql.Statement sql encoder decoder True
  where
    sql = "INSERT INTO users (email, password, name, phone) VALUES ($1, $2, $3, $4) RETURNING id"
    encoder =
      contramap (\(a,b,c,d) -> (a,(b,(c,d)))) $
        Encoders.param (Encoders.nonNullable Encoders.text) <>
        Encoders.param (Encoders.nonNullable Encoders.bytea) <>
        Encoders.param (Encoders.nonNullable Encoders.text) <>
        Encoders.param (Encoders.nullable Encoders.text)
    decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4))

getUserNotifications :: Hasql.Statement Int32 (Vector (Int32, Text, Bool))
getUserNotifications =
  Hasql.Statement sql encoder decoder True
  where
    sql = "SELECT id, message, read FROM notifications WHERE user_id = $1"
    encoder = Encoders.param (Encoders.nonNullable Encoders.int4)
    decoder = Decoders.rowVector $
      (,,) <$>
        Decoders.column (Decoders.nonNullable Decoders.int4) <*>
        Decoders.column (Decoders.nonNullable Decoders.text) <*>
        Decoders.column (Decoders.nonNullable Decoders.bool)
```

Key points:
- `$1`, `$2` placeholders for parameters (PostgreSQL prepared statements)
- Encoder maps Haskell tuple → PostgreSQL binary format
- Decoder maps PostgreSQL result → Haskell type
- Last `Bool` parameter enables statement caching
- **Codecs stay with SQL**: Encoders/decoders defined in same module as queries

### Layer 2: Transactions (Business Logic)

Transactions compose statements with atomicity:

```haskell
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Sessions

register :: Text -> ByteString -> Text -> Maybe Text -> Transaction (Bool, Int32)
register email password name phone = do
  existingId <- Transaction.statement email Statement.findUserByEmail
  case existingId of
    Just id -> pure (False, id)  -- User already existed
    Nothing -> do
      newId <- Transaction.statement (email, password, name, phone) Statement.insertUser
      pure (True, newId)         -- New user created
```

The `(Bool, Int32)` return type is idempotent: callers know whether the operation created a new record.

**Alternative: Applicative/Selective composition** (for independent operations):
```haskell
import Control.Selective (fromMaybeS)

register' :: Text -> ByteString -> Text -> Maybe Text -> Transaction (Bool, Int32)
register' email password name phone =
  fromMaybeS
    (fmap (\newId -> (True, newId)) $
      Transaction.statement (email, password, name, phone) Statement.insertUser)
    (fmap (\existingId -> (False, existingId)) $
      Transaction.statement email Statement.findUserByEmail)
```

### Layer 3: Sessions (Public API)

Sessions wrap transactions and manage connections:

```haskell
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Sessions

-- Using transactions (for multi-statement operations)
registerUser :: Text -> ByteString -> Text -> Maybe Text -> Session (Bool, Int32)
registerUser email password name phone =
  Sessions.transaction Sessions.Write Sessions.Serializable $
    Transaction.register email password name phone

-- Direct statement (for single-statement operations)
authenticate :: Text -> ByteString -> Session (Maybe (Bool, Int32))
authenticate email password =
  Session.statement (email, password) Statement.authenticateUser

getUserDetails :: Int32 -> Session (Maybe (Text, Text, Maybe Text, Bool))
getUserDetails userId =
  Session.statement userId Statement.getUserDetails

getNotifications :: Int32 -> Session (Vector (Int32, Text, Bool))
getNotifications userId =
  Session.statement userId Statement.getUserNotifications
```

Transaction modes:
- `Write Serializable`: For critical operations (registration, payments) - full isolation
- `Read ReadCommitted`: For queries - weaker isolation, better concurrency

### Result Type Conventions

| Pattern | Type | Use Case |
|---------|------|----------|
| Optional row | `Maybe a` | `SELECT` returning 0 or 1 row |
| Multiple rows | `Vector a` | `SELECT` returning many rows |
| Multi-column | `(a, b, c)` | Tuples, not custom wrapper types |
| Success flag | `Bool` | `UPDATE`/`DELETE` success |
| Idempotent | `(Bool, a)` | Indicates if operation was performed |

**Why tuples over custom types?**
- Less boilerplate
- Results are internal to the library anyway
- Consumers see semantic Session API, not raw tuples

### Encoder Composition

For multi-parameter statements, compose encoders with `<>`:

```haskell
-- For (Text, ByteString, Int32)
encoder =
  contramap (\(a,b,c) -> (a,(b,c))) $
    Encoders.param (Encoders.nonNullable Encoders.text) <>
    Encoders.param (Encoders.nonNullable Encoders.bytea) <>
    Encoders.param (Encoders.nonNullable Encoders.int4)
```

The `contramap` restructures the flat tuple into nested pairs for the `<>` combinator.

### Decoder Composition

```haskell
-- Single optional row with multiple columns
Decoders.rowMaybe $
  (,,,) <$>
    Decoders.column (Decoders.nonNullable Decoders.text) <*>
    Decoders.column (Decoders.nonNullable Decoders.text) <*>
    Decoders.column (Decoders.nullable Decoders.text) <*>
    Decoders.column (Decoders.nonNullable Decoders.bool)

-- Vector of rows (multiple results)
Decoders.rowVector $
  (,,) <$>
    Decoders.column (Decoders.nonNullable Decoders.int4) <*>
    Decoders.column (Decoders.nonNullable Decoders.text) <*>
    Decoders.column (Decoders.nonNullable Decoders.bool)

-- Single required row
Decoders.singleRow $
  Decoders.column (Decoders.nonNullable Decoders.int4)
```

### Dependencies

```cabal
build-depends:
    hasql >= 1.4 && < 1.5
  , hasql-transaction >= 0.10 && < 0.11
  , rerebase >= 1.4 && < 1.5  -- Enhanced prelude with Text, Vector, etc.
```

Note: `hasql-transaction` is marked experimental but widely used.

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
