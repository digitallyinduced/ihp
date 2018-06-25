Foundation
==========

## Setup

1. Install the nix package manager: `curl https://nixos.org/nix/install | sh`
2. Install direnv via homebrew: `brew install direnv`
3. Run `make` in the project root. This will launch the database server and the webserver. You can visit the app at `http://localhost:8000/`.

## Useful Commands
Create a ghci instance with all files loaded. Useful to manually regenerate files.
```
make ghci
```
Regenerate Nix-Environment 
```
make -B .envrc
```

# Generators

## Adding a view

To create `src/View/Widgets/Modal.hs` run:
```
$ gen/view Widgets Modal
```

In case there is no controller with the given name, the generator will abort.

# ORM

You can compose database queries using our QueryBuilder module.

## Creating a new query
To query the database for some records, you first need to build a query.
You can just use the `query` function for that.

```
let myQueryBuilder = query
```

You can optionally specify the model you want to query:

```˘
let myProjectQueryBuilder = query @Project
```

## Running a query

You can run a query using `fetch`, `fetchOneOrNothing` or `fetchOne`:

### many rows: `fetch`
To run a query which will return many rows use `fetch`:
```˘
example :: IO [Project]
example = do
    projects <- query @Project |> fetch
    -- Query: `SELECT * FROM projects`
    return projects
```

### maybe single row: `fetchOneOrNothing`
To run a query which will maybe return a single row use `fetchOneOrNothing`:
```˘
example :: IO (Maybe Project)
example = do
    project <- query @Project |> fetchOneOrNothing
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
```

### single row: `fetchOne`
To run a query which will return a single and **throws an error if no record is found** row use `fetchOne`:
```˘
example :: IO Project
example = do
    project <- query @Project |> fetchOne
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
```

## Where Conditions

To specify `WHERE` conditions, you can use `filterWhere`:

```
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
```
projects <- query @Project
        |> orderBy #createdAt
        |> fetch
-- Query: `SELECT * FROM projects ORDER BY created_at`
```

## Or

```
projects <- query @Project
         |> queryOr
            (filterWhere (#userId, userId)) (filterWhere (#teamId, teamId))
        |> fetch
-- Query: `SELECT * FROM projects WHERE (user_id = ?) OR (team_id = ?)`
```

## Union / Merging two queries

Two query builders of the same type can be merged like this:

```
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

```
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOne
-- Shorter version
project <- query @Project |> findBy #userId userId
```

### `findMaybeBy #field value`
Just a shortcut for `filterWhere (#field, value) |> fetchOneOrNothing`

```
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOneOrNothing
-- Shorter version
project <- query @Project |> findMaybeBy #userId userId
```

### `findById id`
Just a shortcut for `filterWhere (#id, id) |> fetchOne`

```
-- Long version
project <- query @Project |> filterWhere (#id, id) |> fetchOne
-- Shorter version
project <- query @Project |> findOneById #id id
```

### `findManyBy #field value`
Just a shortcut for `filterWhere (#field, value) |> fetch`

```
-- Long version
projects <- query @Project |> filterWhere (#userId, userId) |> fetch
-- Shorter version
projects <- query @Project |> findManyBy #userId userId
```

# Form

```
formFor model modelsPath [hsx|
    {textField #name}
|]
```

## Select Inputs

You can use the `selectField` helper for select inputs:
```
formFor project projectsPath [hsx|
    {selectField #userId users}
|]
```
In the example above the variable `users` contains all the possible option values for the select.

You also need to define a instance `CanSelect User`:
```
instance CanSelect User where
    -- Here we specify that the <option>-value should contain a UserId
    type SelectValue User = UserId
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
```

Given the above example, the rendered form will look like this:
```
-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }]
<form ...>
    <select name="user_id">
        <option value="1">Marc</option>
        <option value="2">Andreas</option>
    </select>
</form>
```
