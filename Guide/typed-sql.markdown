# Typed SQL

```toc

```

## Introduction

IHP's query builder covers most common database operations. But as your application grows, you'll encounter complex queries that are hard to express with the query builder. The standard escape hatch is `sqlQuery`, which executes raw SQL but decodes results at runtime - meaning type errors only show up when the code runs.

The `typedSql` quasiquoter solves this: it connects to your development database at compile time, asks PostgreSQL to describe your query, and generates type-safe Haskell code. You get:

- **Compile-time type inference**: Parameter and result types are inferred from your SQL
- **No manual `FromRow` instances**: Result decoders are generated automatically
- **Type errors at compile time**: If your SQL doesn't match your Haskell types, you get a compile error instead of a runtime crash

## Getting Started

Add `ihp-typed-sql` to your project's dependencies and import the module:

```haskell
import IHP.TypedSql (typedSql, sqlQueryTyped, sqlExecTyped)
```

The `QuasiQuotes` extension is required but already enabled by default in IHP projects.

**Important**: Your development database must be running during compilation, because `typedSql` uses `DATABASE_URL` to connect and describe queries at compile time. For `nix build`, this is handled automatically — see [Production Builds](#production-builds) below.

## Basic Queries

A simple SELECT returning a single column:

```haskell
action ItemsAction = do
    names <- sqlQueryTyped [typedSql|
        SELECT name FROM items
        WHERE views > ${minViews}
        ORDER BY name
    |]

    -- names :: [Text]
    render IndexView { names }
```

The `typedSql` quasiquoter produces a `TypedQuery result` value. Use `sqlQueryTyped` to execute it and get back a list of results.

## Selecting Multiple Columns

When selecting multiple columns, the result is a tuple:

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT id, name, views FROM items LIMIT 10
|]

-- rows :: [(Id' "items", Text, Int)]
forEach rows \(itemId, name, views) -> do
    putStrLn (name <> ": " <> show views)
```

Primary key columns are automatically typed as `Id' "table_name"` rather than raw `UUID`.

## Selecting All Columns

### Why `SELECT *` is disallowed by default

`SELECT *` and `SELECT table.*` are not allowed in `typedSql` by default. At compile time, `*` is expanded to whatever columns exist in the development database and a decoder is built for those exact columns. If the production database has a different schema (e.g., a migration added or removed a column), the query will return different columns than the decoder expects, causing a runtime error.

Instead, list columns explicitly:

```haskell
items <- sqlQueryTyped [typedSql|
    SELECT id, name, views FROM items ORDER BY name
|]
```

The compile error message will suggest the exact column names to use.

### Opting in with `typedSqlStar`

If you understand the risk and want to use `table.*` anyway (e.g., during rapid prototyping), use the `typedSqlStar` quasiquoter:

```haskell
items <- sqlQueryTyped [typedSqlStar|
    SELECT items.* FROM items ORDER BY name
|]

-- items :: [Item]
```

This requires a `FromRowHasql` instance on the model type. IHP's generated types include this instance automatically.

Table aliases work too:

```haskell
items <- sqlQueryTyped [typedSqlStar|
    SELECT i.* FROM items i
    JOIN authors a ON a.id = i.author_id
    ORDER BY i.name
|]
```

## Inserting Rows

### Why `INSERT … VALUES` without a column list is disallowed by default

`INSERT INTO table VALUES (...)` and `INSERT INTO table SELECT ...` without an explicit column list are not allowed in `typedSql` by default. They rely on the positional order of columns matching the schema, but column order can drift between development and production (e.g., when migrations are applied in a different sequence). This causes values to be silently inserted into the wrong columns at runtime.

Instead, list the target columns explicitly:

```haskell
sqlExecTyped [typedSql|
    INSERT INTO items (id, name, views)
    VALUES (${itemId}, ${name}, ${views})
|]
```

`INSERT INTO table DEFAULT VALUES` is allowed since it has no positional binding.

The same `[typedSqlStar| ... |]` escape hatch applies here if you understand the risk.

## Parameters

Use `${expr}` to splice Haskell expressions into your SQL as parameters:

### Simple Parameters

```haskell
let minViews = 10 :: Int
names <- sqlQueryTyped [typedSql|
    SELECT name FROM items WHERE views > ${minViews}
|]
```

The parameter type is inferred from the column it's compared against. If `views` is an `int4` column, `minViews` must be an `Int`.

### Foreign Key Parameters

Foreign key parameters are automatically coerced to the correct `Id` type:

```haskell
let authorId = "some-uuid" :: Id' "authors"
names <- sqlQueryTyped [typedSql|
    SELECT name FROM items WHERE author_id = ${authorId}
|]
```

### IN Lists

Pass a Haskell list to use with `IN`:

```haskell
let itemIds = [itemId1, itemId2] :: [Id' "items"]
names <- sqlQueryTyped [typedSql|
    SELECT name FROM items WHERE id IN (${itemIds})
|]
```

### ANY Arrays

Alternatively, use `ANY` with an array parameter:

```haskell
let itemIds = [itemId1, itemId2] :: [Id' "items"]
names <- sqlQueryTyped [typedSql|
    SELECT name FROM items WHERE id = ANY(${itemIds})
|]
```

## INSERT / UPDATE / DELETE

Use `sqlExecTyped` for write operations. It returns `Int64` (the number of affected rows):

```haskell
rowsInserted <- sqlExecTyped [typedSql|
    INSERT INTO items (id, author_id, name, views, tags)
    VALUES (${itemId}, ${authorId}, ${name}, ${views}, ${tags})
|]

rowsDeleted <- sqlExecTyped [typedSql|
    DELETE FROM items WHERE views < ${minViews}
|]
```

## Nullability

Typed SQL automatically determines whether result columns should be wrapped in `Maybe`:

### Table Columns

Nullable table columns are wrapped in `Maybe`, non-nullable columns are not:

```haskell
-- score is a nullable DOUBLE PRECISION column
scores <- sqlQueryTyped [typedSql| SELECT score FROM items |]
-- scores :: [Maybe Double]

-- name is a NOT NULL TEXT column
names <- sqlQueryTyped [typedSql| SELECT name FROM items |]
-- names :: [Text]
```

### Computed Expressions

Computed expressions (aggregates, CASE, arithmetic, literals, etc.) are always wrapped in `Maybe`, because PostgreSQL cannot guarantee they are non-null:

```haskell
counts <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM items |]
-- counts :: [Maybe Int64]

results <- sqlQueryTyped [typedSql|
    SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM items
|]
-- results :: [Maybe Text]

literals <- sqlQueryTyped [typedSql| SELECT 1 |]
-- literals :: [Maybe Int]
```

### Primary and Foreign Keys

Primary keys are typed as `Id' "table_name"` (not raw `UUID`):

```haskell
ids <- sqlQueryTyped [typedSql| SELECT id FROM items |]
-- ids :: [Id' "items"]
```

Nullable foreign keys are `Maybe (Id' "referenced_table")`:

```haskell
authorIds <- sqlQueryTyped [typedSql| SELECT author_id FROM items |]
-- authorIds :: [Maybe (Id' "authors")]
```

## JOINs

Join nullability is tracked automatically:

### INNER JOIN

Both sides are non-nullable:

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT i.name, a.name
    FROM items i
    INNER JOIN authors a ON a.id = i.author_id
|]
-- rows :: [(Text, Text)]
```

### LEFT JOIN

Right-side columns are wrapped in `Maybe`:

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT i.name, a.name
    FROM items i
    LEFT JOIN authors a ON a.id = i.author_id
|]
-- rows :: [(Text, Maybe Text)]
```

### RIGHT JOIN

Left-side columns are wrapped in `Maybe`:

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT i.name, a.name
    FROM items i
    RIGHT JOIN authors a ON a.id = i.author_id
|]
-- rows :: [(Maybe Text, Text)]
```

## Advanced Queries

### CTEs (Common Table Expressions)

```haskell
rows <- sqlQueryTyped [typedSql|
    WITH popular AS (
        SELECT name FROM items WHERE views > 100
    )
    SELECT name FROM popular ORDER BY name
|]
-- rows :: [Text]
```

### Subqueries

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT name FROM (
        SELECT name FROM items WHERE views < 10
    ) sub
    ORDER BY name
|]
-- rows :: [Text]
```

### UNION ALL

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT name FROM items WHERE views > 100
    UNION ALL
    SELECT name FROM items WHERE views < 10
|]
-- rows :: [Maybe Text]
```

Note: UNION results are typed as `Maybe` because PostgreSQL reports them as computed columns.

### Window Functions

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT name, row_number() OVER (ORDER BY views DESC)
    FROM items
|]
-- rows :: [(Text, Maybe Int64)]
```

### GROUP BY with Aggregates

```haskell
rows <- sqlQueryTyped [typedSql|
    SELECT name, COUNT(*)
    FROM items
    GROUP BY name
    ORDER BY name
|]
-- rows :: [(Text, Maybe Int64)]
```

## Type Mapping Reference

The following table shows how PostgreSQL types map to Haskell types:

| PostgreSQL Type | Haskell Type |
|---|---|
| `int2`, `int4` | `Int` |
| `int8` | `Int64` |
| `text`, `varchar`, `bpchar`, `citext` | `Text` |
| `bool` | `Bool` |
| `uuid` | `UUID` (or `Id' "table"` for primary/foreign keys) |
| `timestamptz` | `UTCTime` |
| `timestamp` | `LocalTime` |
| `date` | `Day` |
| `time` | `TimeOfDay` |
| `json`, `jsonb` | `Aeson.Value` |
| `bytea` | `ByteString` |
| `float4` | `Float` |
| `float8` | `Double` |
| `numeric` | `Scientific` |
| `point` | `Point` |
| `polygon` | `Polygon` |
| `inet` | `Inet` |
| `tsvector` | `Tsvector` |
| `interval` | `Interval` |
| `anytype[]` | `[ElementType]` (arrays map to lists) |
| Custom enums | Corresponding Haskell type |

## Compared to Raw SQL (`sqlQuery`)

The standard `sqlQuery` function executes raw SQL with runtime type decoding:

```haskell
-- sqlQuery: types checked at runtime
names <- sqlQuery "SELECT name FROM items WHERE views > ?" (Only minViews)

-- typedSql: types checked at compile time
names <- sqlQueryTyped [typedSql| SELECT name FROM items WHERE views > ${minViews} |]
```

Key differences:

| | `sqlQuery` | `typedSql` |
|---|---|---|
| Type checking | Runtime | Compile time |
| Parameters | `?` placeholders + tuple | `${expr}` inline expressions |
| `FromRow` instances | Required (manual or derived) | Generated automatically |
| Database at compile time | Not needed | Required |

**When to use `sqlQuery`**: Simple queries where you already have `FromRow` instances, or when you can't have the database running during compilation.

**When to use `typedSql`**: Complex queries, queries with many columns, or any time you want compile-time safety. Especially useful when the query shape changes frequently during development.

### Production Builds

When you run `nix build`, IHP automatically detects that `ihp-typed-sql` is in your dependencies and starts a temporary PostgreSQL instance during compilation. Your `Application/Schema.sql` is loaded into this temporary database so that `typedSql` can infer types at compile time — no extra configuration needed.

### Migrating from `sqlQuery` to `typedSql`

1. Replace `sqlQuery` with `sqlQueryTyped` (or `sqlExec` with `sqlExecTyped`)
2. Wrap the SQL string in `[typedSql| ... |]`
3. Replace `?` placeholders with `${expr}` expressions
4. Remove the parameter tuple - parameters are now inline
5. Remove any `FromRow` instances that were only needed for that query
