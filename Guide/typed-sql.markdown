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
- enums -> `Generated.Enums.<Enum>`
- composite types -> `Generated.ActualTypes.<Type>`

If you have custom types, add a `FromField` instance and extend
`hsTypeForPg` in `IHP.TypedSql`.

## Runtime behavior

`runTyped` uses IHP's `ModelContext`, so it automatically:

- uses the pooled connection
- respects row-level security (RLS)
- logs queries in the same format as `sqlQuery`

There is no separate runtime connection layer.

## Compile-time database access

`typedSql` talks to your database at compile time. It uses
`DATABASE_URL` or the same default used by IHP (`build/db`). Make sure
the database is running and the schema is up to date when compiling.

If the schema changes, recompile so the query description is refreshed.

## Tests without a database: stub mode

For tests, you can skip the live DB by providing a JSON stub file. This
lets you run `typedSql` in CI without a running Postgres.

Set the environment variable before splicing any `typedSql`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH.Syntax (runIO)
import System.Environment (setEnv)

$(do
    runIO (setEnv "IHP_TYPED_SQL_STUB" "Test/Test/TypedSqlStub.json")
    pure []
 )
```

### Stub file format

A stub file is a JSON document with a list of query entries. Each entry
contains:

- the SQL string (after placeholder substitution)
- parameter OIDs
- column OIDs and table metadata
- table metadata (columns, PKs, FKs)
- type metadata (names, element OIDs, type category)

Example:

```json
{
  "queries": [
    {
      "sql": "SELECT users.id, users.name FROM users WHERE users.id = $1",
      "params": [2950],
      "columns": [
        {"name": "id", "typeOid": 2950, "tableOid": 100000, "attnum": 1},
        {"name": "name", "typeOid": 25, "tableOid": 100000, "attnum": 2}
      ],
      "tables": [
        {
          "oid": 100000,
          "name": "users",
          "columns": [
            {"attnum": 1, "name": "id", "typeOid": 2950, "notNull": true},
            {"attnum": 2, "name": "name", "typeOid": 25, "notNull": true}
          ],
          "primaryKeys": [1],
          "foreignKeys": []
        }
      ],
      "types": [
        {"oid": 2950, "name": "uuid", "elemOid": null, "typtype": "b"},
        {"oid": 25, "name": "text", "elemOid": null, "typtype": "b"}
      ]
    }
  ]
}
```

Only the queries your tests use need to be present in the stub file.

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
- Or set `IHP_TYPED_SQL_STUB` to use stub mode.

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
