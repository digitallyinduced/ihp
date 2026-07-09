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

**Important**: `typedSql` describes queries against PostgreSQL during compilation. It first uses `DATABASE_URL`, so an already-running `devenv up` database is preferred. In the IHP dev shell, `typedSql` can also start a temporary private PostgreSQL automatically for non-interactive typechecking, e.g. in coding-agent workspaces where `devenv up` is not running. For `nix build`, this is handled automatically — see [Production Builds](#production-builds) below.

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

The `typedSql` quasiquoter produces a `TypedQuery cardinality result` value. Use `sqlQueryTyped` to execute it. The return shape follows the cardinality that can be proven from the SQL:

- many rows: `[result]`
- at most one row, e.g. `LIMIT 1` or a primary-key lookup: `Maybe result`
- exactly one row, e.g. `COUNT(*)`, `SELECT 1`, or `EXISTS(...)`: `result`

```haskell
total <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM items |]
-- total :: Int64

maybeName <- sqlQueryTyped [typedSql| SELECT name FROM items ORDER BY name LIMIT 1 |]
-- maybeName :: Maybe Text
```

If you want the expected shape in the function name, use
`sqlQueryTypedRows`, `sqlQueryTypedOneOrNothing`, and
`sqlQueryTypedSingle`:

```haskell
names <- sqlQueryTypedRows [typedSql| SELECT name FROM items ORDER BY name |]
-- names :: [Text]

maybeName <- sqlQueryTypedOneOrNothing [typedSql|
    SELECT name FROM items ORDER BY name LIMIT 1
|]
-- maybeName :: Maybe Text

total <- sqlQueryTypedSingle [typedSql| SELECT COUNT(*) FROM items |]
-- total :: Int64
```

For nullable single-column queries that return at most one row, the precise
result shape is `Maybe (Maybe a)`: the outer `Maybe` is "no row", the inner
`Maybe` is "the SQL value was NULL". Use `sqlQueryTypedMaybeColumn` when you
want both cases collapsed into `Nothing`:

```haskell
score <- sqlQueryTypedMaybeColumn [typedSql|
    SELECT score FROM items WHERE id = ${itemId}
|]
-- score :: Maybe Double
```

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

Use `sqlExecTyped` for write operations. It returns `Int64` (the number of affected rows). For typed statements where PostgreSQL does not provide an affected-row count, such as `SET CONSTRAINTS`, it returns `0` after the statement succeeds:

```haskell
rowsInserted <- sqlExecTyped [typedSql|
    INSERT INTO items (id, author_id, name, views, tags)
    VALUES (${itemId}, ${authorId}, ${name}, ${views}, ${tags})
|]

rowsDeleted <- sqlExecTyped [typedSql|
    DELETE FROM items WHERE views < ${minViews}
|]

_ <- sqlExecTyped [typedSql|
    SET CONSTRAINTS ALL DEFERRED
|]
```

## Pagination

Use `paginatedTypedSql` to paginate a typedSql query. It takes a `TypedQuery` and returns a list of records together with a `Pagination` state, mirroring IHP's other paginators:

```haskell
import IHP.TypedSql.Pagination (paginatedTypedSql, paginatedTypedSqlWithOptions)

action ItemsAction = do
    (items, pagination) <- paginatedTypedSql [typedSql|
        SELECT id, name, views FROM items ORDER BY name
    |]
    render IndexView { items, pagination }
```

Pass the `pagination` value to your view and call `renderPagination` there, exactly as with `paginate`. Use `paginatedTypedSqlWithOptions` to override the defaults (e.g. items per page):

```haskell
(items, pagination) <- paginatedTypedSqlWithOptions
    (defaultPaginationOptions |> set #maxItems 10)
    [typedSql| SELECT id, name, views FROM items ORDER BY name |]
```

Because the query is wrapped in a subquery before `LIMIT`/`OFFSET` are applied, any `ORDER BY` must live **inside** the query you pass in. See the [Pagination guide](pagination.html#typed-sql-pagination) for the full details.

## Pipeline Mode

Use `sqlQueryTypedPipelined` together with `IHP.FetchPipelined.pipeline` to run
independent typed SQL queries in a single PostgreSQL pipeline batch:

```haskell
import IHP.FetchPipelined (pipeline)
import IHP.TypedSql (sqlQueryTypedPipelined, typedSql)

action DashboardAction = do
    (names, total) <- pipeline do
        names <- sqlQueryTypedPipelined [typedSql|
            SELECT name FROM items ORDER BY name LIMIT 10
        |]
        total <- sqlQueryTypedPipelined [typedSql|
            SELECT COUNT(*) FROM items
        |]
        pure (names, total)

    -- names :: [Text]
    -- total :: Int64
    render DashboardView { names, total }
```

For nullable single-column queries in a pipeline, use
`sqlQueryTypedMaybeColumnPipelined`.

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

Computed expressions (aggregates, CASE, arithmetic, literals, etc.) are wrapped in `Maybe` unless `typedSql` can prove they are non-null:

```haskell
count <- sqlQueryTyped [typedSql| SELECT COUNT(*) FROM items |]
-- count :: Int64

results <- sqlQueryTyped [typedSql|
    SELECT CASE WHEN views > 5 THEN name ELSE 'low' END FROM items
|]
-- results :: [Maybe Text]

literal <- sqlQueryTyped [typedSql| SELECT 1 |]
-- literal :: Int
```

IHP also corrects conservative cases where PostgreSQL reports computed
expressions as nullable even though a value is guaranteed, including
`COUNT(*)`, `EXISTS`, non-null literals, window ranking functions, `COALESCE`
with a known non-null argument, and JSON constructors like
`json_build_object`, `jsonb_build_object`, `json_build_array`, and
`jsonb_build_array`.

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
