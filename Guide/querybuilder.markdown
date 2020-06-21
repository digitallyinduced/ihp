# QueryBuilder

```toc
```

## Introduction

The QueryBuilder module allows you to compose database queries in a type-safe way. Below you find a short reference to all the commonly used functions.

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
    return projects
```

### maybe single row: `fetchOneOrNothing`
To run a query which will maybe return a single row use `fetchOneOrNothing`:
```haskell
example :: IO (Maybe Project)
example = do
    project <- query @Project |> fetchOneOrNothing
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
```

### single row: `fetchOne`
To run a query which will return a single and **throws an error if no record is found** row use `fetchOne`:
```haskell
example :: IO Project
example = do
    project <- query @Project |> fetchOne
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
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
    return projects
```

## Order By

You can just use `orderBy #field`:
```haskell
projects <- query @Project
        |> orderBy #createdAt
        |> fetch
-- Query: `SELECT * FROM projects ORDER BY created_at`
```

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

For convience there is also a `fetch` implementation for `Maybe SomeId`:

```haskell
let assignedUserId :: Maybe UserId = project |> get #assignedUserId
assignedUser <- assignedUserId |> fetchOneOrNothing
```

