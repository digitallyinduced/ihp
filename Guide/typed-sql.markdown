# Typed SQL (typedSql)

`typedSql` gives you compile-time checked SQL that integrates with IHP
models. It is designed for cases where Query Builder is too limited and
`sqlQuery` is too loose. The goal is to let you keep raw SQL while still
getting:

- compile-time validation (syntax and type checks)
- automatic mapping to IHP's generated types
- column name and nullability handling consistent with IHP

This page explains how it works, how to use it, and how to debug it.

## When to use typedSql

Use `typedSql` when:

- you need complex joins, CTEs, window functions, or vendor-specific SQL
- you want a typed result without writing `FromRow` manually
- you want compile-time SQL checking against your schema

Keep using Query Builder for simple filtering and `sqlQuery` for truly
runtime-generated SQL.

## Quick start

Enable Template Haskell and QuasiQuotes in a module where you want to use
`typedSql`:

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
```

Import the module and run the query using `runTyped`:

```haskell
module Web.Controller.Users where

import IHP.ControllerPrelude
import IHP.TypedSql

indexAction :: ?modelContext => IO ()
indexAction = do
    let userId = "00000000-0000-0000-0000-000000000001" :: Id User
    users <- runTyped [typedSql|
        SELECT users.id, users.name
        FROM users
        WHERE users.id = ${userId}
    |]

    render Json { users }
```

`typedSql` produces a `TypedQuery` value that is executed using
`runTyped` or `runTypedOne`.

## Result type inference

### Full table row

If the query returns all columns of a table in the exact order defined in
the schema (e.g. `SELECT users.*`), then the result is the generated
record type:

```haskell
user :: User <- runTypedOne [typedSql|
    SELECT users.* FROM users WHERE users.id = ${userId}
|]
```

### Partial selection

When you return a subset of columns, the result is a tuple:

```haskell
userInfo :: [(Id User, Text)] <- runTyped [typedSql|
    SELECT users.id, users.name FROM users
|]
```

### Foreign keys

If a column is a single-column foreign key, `typedSql` maps it to
`Id' "other_table"` automatically:

```haskell
authorIds :: [Maybe (Id User)] <- runTyped [typedSql|
    SELECT posts.author_id FROM posts WHERE posts.slug = ${slug}
|]
```

This follows IHP's usual `Id` mapping rules.

## Parameter placeholders

`typedSql` uses `${expr}` placeholders. Each placeholder becomes a `$N`
parameter and is type-checked against the database.

```haskell
runTyped [typedSql|
    SELECT * FROM posts WHERE posts.id = ${postId}
|]
```

Notes:

- Do not use `?` or `$1` placeholders directly.
- Parameter types come from OIDs only, so UUID parameters are `UUID` (not
  `Id'`). Use `get #id record` or `unId` if you want to pass an `Id'`.
- Use explicit type annotations for ambiguous values:

```haskell
runTyped [typedSql|
    SELECT * FROM posts WHERE posts.score > ${10 :: Int}
|]
```

- For arrays, prefer `= ANY(${ids})` rather than `IN (${ids})`.

## Nullability rules

`typedSql` tries to infer nullability from `pg_attribute` when a column is
traceable to a table. If a column comes from an expression or a `LEFT
JOIN`, the result is treated as nullable (`Maybe`) by default.

If you want to force a non-null result, use SQL functions such as
`COALESCE`:

```haskell
runTyped [typedSql|
    SELECT COALESCE(posts.title, '') FROM posts
|]
```

## Type mapping

The mapping follows IHP's conventions. Summary of common types:

- `uuid` -> `UUID` (result columns from PK/FK map to `Id' "table"`)
- `text`, `varchar`, `bpchar`, `citext` -> `Text`
- `int2`, `int4` -> `Int`
- `int8` -> `Integer`
- `bool` -> `Bool`
- `timestamptz` -> `UTCTime`
- `timestamp` -> `LocalTime`
- `date` -> `Day`
- `time` -> `TimeOfDay`
- `json`, `jsonb` -> `Aeson.Value`
- `bytea` -> `Binary ByteString`
- `numeric` -> `Scientific`
- `interval` -> `PGInterval`
- `point` -> `Point`
- `polygon` -> `Polygon`
- `inet` -> `IP`
- `tsvector` -> `TSVector`
- enums -> `<Enum>` (re-exported from `Generated.Types`)
- composite types -> `<Type>` (re-exported from `Generated.Types`)

Single-column composite selects (e.g. `SELECT my_table FROM my_table`) are
not supported because `postgresql-simple` cannot decode composite fields
into record types. Use `SELECT my_table.*` or list columns explicitly.

If you have custom types, add a `FromField` instance and extend
`hsTypeForPg` in `IHP.TypedSql`.

## Runtime behavior

`runTyped` uses IHP's `ModelContext`, so it automatically:

- uses the pooled connection
- respects row-level security (RLS)
- logs queries in the same format as `sqlQuery`

There is no separate runtime connection layer.

## How typedSql works internally

`typedSql` is implemented as a Template Haskell quasiquoter. The pipeline
looks like this:

1. **Placeholder rewrite**: the SQL template is scanned for `${expr}`
   placeholders. Each placeholder is replaced by `$1`, `$2`, ... for the
   compile-time describe and by `?` for runtime execution. The captured
   expressions are parsed as Haskell AST.
2. **Statement describe**: at compile time, `typedSql` prepares the query
   and runs `DESCRIBE` via libpq. This returns:
   - parameter OIDs (types for each `$N`)
   - result column OIDs, table OIDs, and attribute numbers
3. **Catalog metadata fetch**: `typedSql` then queries `pg_catalog` to
   resolve:
   - table/column order (`pg_class`, `pg_attribute`)
   - nullability (`pg_attribute.attnotnull`)
   - primary/foreign keys (`pg_constraint`)
   - enum/composite/array metadata (`pg_type`)
4. **IHP type mapping**:
   - Primary keys become `Id' "table"`.
   - Single-column foreign keys become `Id' "ref_table"`.
   - Enums map to `<Enum>` (re-exported from `Generated.Types`).
   - Composite types map to `<Type>` (re-exported from `Generated.Types`).
   - If the select list exactly matches `table.*` order, the result type
     becomes the generated record type (`<Model>` from `Generated.Types`).
5. **TypedQuery generation**: the quasiquoter emits a `TypedQuery` value
   with:
   - `PG.Query` containing the rewritten SQL
   - `toField`-encoded parameters
   - a row parser (`field` for single column, `fromRow` for tuples)

At runtime, `runTyped` executes the generated query using IHP's
`ModelContext`, so it reuses the same logging, RLS parameters, and
connection pool as the rest of IHP.

## Compile-time database access

`typedSql` needs schema metadata at compile time. If
`IHP_TYPED_SQL_BOOTSTRAP` is set, it uses bootstrap mode. Otherwise it
connects to your live database.

### Live database (default)

`typedSql` connects to your database at compile time using `DATABASE_URL`
or the same default used by IHP (`build/db`). Make sure the database is
running and the schema is up to date when compiling.

If the schema changes, recompile so the query description is refreshed.

### Bootstrap mode (schema-only)

Bootstrap mode avoids a running database by creating a temporary local
Postgres instance from your SQL files at compile time. This keeps the
SQL/type checking fully real while remaining reproducible in CI.

Enable it with:

```bash
export IHP_TYPED_SQL_BOOTSTRAP=1
```

When enabled, `typedSql` will:

1. Run `initdb` into a temp directory.
2. Start a local `postgres` instance bound to a unix socket.
3. Load `IHPSchema.sql` (if found), then `Application/Schema.sql`.
4. Run the same describe + catalog queries against the temporary DB.

Schema discovery rules:

- `IHP_TYPED_SQL_SCHEMA` overrides the app schema path.
- Otherwise, `Application/Schema.sql` is discovered by walking upward from
  the module containing the `[typedSql| ... |]`.
- `IHP_TYPED_SQL_IHP_SCHEMA` overrides the IHP schema path.
- Otherwise, if `IHP_LIB` is set, `IHP_LIB/IHPSchema.sql` is used.
- Otherwise, it tries to locate `ihp-ide/data/IHPSchema.sql` when building
  from the IHP repo.

Tools required on `PATH`:

- `initdb`
- `postgres`
- `createdb`
- `psql`

If any tool is missing, `typedSql` will fail with a clear error.

## Limitations and gotchas

- Only `${expr}` placeholders are supported.
- Queries with untracked parameters (e.g. `$1` without `${}`) will fail.
- Multi-column foreign keys are not mapped to `Id` yet.
- Nullability for computed columns defaults to `Maybe`.
- Compile-time checks require a schema that matches the runtime schema.

## Migration guidance

If you currently use `sqlQuery` for complex queries:

1. Wrap the query in `[typedSql| ... |]`.
2. Replace `?` placeholders with `${expr}`.
3. Replace custom `FromRow` with inferred tuples or records.
4. Use `runTyped` instead of `sqlQuery`.

You get compile-time SQL validation with minimal changes.

## Troubleshooting

**Error: could not connect to database**

- Ensure `DATABASE_URL` is set and reachable during compilation.
- Or set `IHP_TYPED_SQL_BOOTSTRAP=1` to use bootstrap mode.

**Error: bootstrap requires 'initdb' in PATH**

- Install the PostgreSQL client/server tools.
- Make sure `initdb`, `postgres`, `createdb`, and `psql` are on `PATH`.

**Error: could not find Application/Schema.sql**

- Set `IHP_TYPED_SQL_SCHEMA` to an absolute path.
- Or ensure the module using `typedSql` is inside your app directory.

**Error: placeholder count mismatch**

- Check that every parameter is written as `${expr}`.

**Unexpected `Maybe` results**

- The column is nullable or computed. Use `COALESCE` or accept `Maybe`.

**Unknown type errors**

- Add an explicit type cast in SQL or add a mapping in `IHP.TypedSql`.

## API summary

```haskell
typedSql :: QuasiQuoter
runTyped :: (?modelContext :: ModelContext) => TypedQuery result -> IO [result]
runTypedOne :: (?modelContext :: ModelContext) => TypedQuery result -> IO result
```

See `IHP.TypedSql` for the full implementation.
