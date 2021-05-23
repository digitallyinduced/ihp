# QueryBuilder

```toc

```

## Introduction

The QueryBuilder module allows you to compose database queries in a type-safe way. Below you can find a short reference to all the commonly-used functions.

## Creating a new query

To query the database for some records, you first need to build a query.
You can just use the `query` function for that.

```haskell
let myQueryBuilder = query
```

You can optionally specify the model you want to query:

```haskell
let myProjectQueryBuilder = query @Project
```

## Running a query

You can run a query using `fetch`, `fetchOneOrNothing` or `fetchOne`:

### many rows: `fetch`

To run a query which will return many rows use `fetch`:

```haskell
example :: IO [Project]
example = do
    projects <- query @Project |> fetch
    -- Query: `SELECT * FROM projects`
    pure projects
```

### maybe single row: `fetchOneOrNothing`

To run a query which will maybe return a single row use `fetchOneOrNothing`:

```haskell
example :: IO (Maybe Project)
example = do
    project <- query @Project |> fetchOneOrNothing
    -- Query: `SELECT * FROM projects LIMIT 1`
    pure project
```

### single row: `fetchOne`

To run a query which will return a single row and **throw an error if no record is found** use `fetchOne`:

```haskell
example :: IO Project
example = do
    project <- query @Project |> fetchOne
    -- Query: `SELECT * FROM projects LIMIT 1`
    pure project
```

## Where Conditions

To specify `WHERE` conditions, you can use `filterWhere`:

```haskell
projectsByUser :: UserId -> IO [Project]
projectsByUser userId = do
    projects <- query @Project
            |> filterWhere (#userId, userId)
            |> filterWhere (#deleted, False)
            |> fetch
    -- Query: `SELECT * FROM projects WHERE user_id = <userId> AND deleted = false`
    pure projects
```

Use `filterWhereNot` to negate a condition:

```haskell
projectsByUser :: UserId -> IO [Project]
projectsByUser userId = do
    otherProjects <- query @Project
            |> filterWhereNot (#userId, userId)
            |> fetch
    -- Query: `SELECT * FROM projects WHERE user_id != <userId>`
    pure otherProjects
```

You can also use the more general `filterWhereSql`:

```haskell
retiredEmployees :: IO [Employee]
retiredEmployees = do
    employees <- query @Employee
             |> filterWhereSql (#retireddate, "IS NOT NULL")
             |> fetch
    -- Query: `SELECT * FROM employee WHERE retireddate IS NOT NULL`
    pure employees
```

Several other filter-functions for generating `WHERE` clauses exist, such as `filterWhereIn` and `filterWhereNotIn` which take lists of items. Read more about these in the [API docs on QueryBuilder](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html)

## Order By

You can just use `orderBy #field`:

```haskell
projects <- query @Project
        |> orderBy #createdAt
        |> fetch
-- Query: `SELECT * FROM projects ORDER BY created_at`
```

Nested `orderBy`s work as expected:

```haskell
projects <- query @Employee
        |> orderBy #lastname
        |> orderBy #firstname
        |> fetch
-- Query: `SELECT * FROM employees ORDER BY lastname, firstname`
```

## Limit

To limit the number of rows returned:

```haskell
projects <- query @Project
        |> limit 10
        |> fetch
-- Query: `SELECT * FROM projects LIMIT 10`
```

## Offset

To skip multiple rows:

```haskell
projects <- query @Project
        |> offset 10
        |> fetch
-- Query: `SELECT * FROM projects OFFSET 10`
```

Offset is most often used together with a limit to implement pagination.

## Or

```haskell
projects <- query @Project
         |> queryOr
            (filterWhere (#userId, userId)) (filterWhere (#teamId, teamId))
        |> fetch
-- Query: `SELECT * FROM projects WHERE (user_id = ?) OR (team_id = ?)`
```

## Union / Merging two queries

Two query builders of the same type can be merged like this:

```haskell
-- SELECT * FROM projects WHERE team_id = ?
let teamProjects :: QueryBuilder Project = query @Project |> filterWhere (#teamId, teamId)

-- SELECT * FROM projects WHERE team_id IS NULL AND created_by = ?
let personalProjects :: QueryBuilder Project = query @Project |> filterWhere (#teamId, Nothing) |> filterWhere (#createdBy, currentUserId)

-- SELECT * FROM projects WHERE (team_id = ?) OR (team_id IS NULL AND created_by = ?)
let projects :: QueryBuilder Project = queryUnion teamProjects personalProjects
```

## Shortcuts

### `findBy #field value`

Just a shortcut for `filterWhere (#field, value) |> fetchOne`

```haskell
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOne
-- Shorter version
project <- query @Project |> findBy #userId userId
```

### `findMaybeBy #field value`

Just a shortcut for `filterWhere (#field, value) |> fetchOneOrNothing`

```haskell
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOneOrNothing
-- Shorter version
project <- query @Project |> findMaybeBy #userId userId
```

### `findManyBy #field value`

Just a shortcut for `filterWhere (#field, value) |> fetch`

```haskell
-- Long version
projects <- query @Project |> filterWhere (#userId, userId) |> fetch
-- Shorter version
projects <- query @Project |> findManyBy #userId userId
```

## `projectId |> fetch`

Ids also have `fetch` implementations, that way you can just run:

```haskell
let projectId :: ProjectId = ...
project <- projectId |> fetch
```

For convenience there is also a `fetch` implementation for `Maybe SomeId`:

```haskell
let assignedUserId :: Maybe UserId = project |> get #assignedUserId
assignedUser <- assignedUserId |> fetchOneOrNothing
```
