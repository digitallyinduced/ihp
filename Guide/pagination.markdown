# Pagination

```toc

```

## Introduction

IHP has built-in support for paginating query results. Pagination automatically handles splitting your data across multiple pages, rendering page navigation controls, and reading the current page from URL query parameters.

The pagination modules are already imported when you use `ControllerPrelude` and `ViewPrelude`, so no extra imports are needed in typical IHP applications.

## Basic Usage

Pagination involves three steps:

1. Call `paginate` on your query in the controller
2. Pass the `Pagination` value to your view
3. Call `renderPagination` in the view

### Controller

Use `paginate` to add pagination to any query. It returns a tuple of the paginated query and a `Pagination` state:

```haskell
action UsersAction = do
    (usersQuery, pagination) <- query @User
        |> orderBy #createdAt
        |> paginate
    users <- usersQuery |> fetch
    render IndexView { .. }
```

The `paginate` function does two things:

1. Reads the `page` query parameter from the URL (defaults to `1`) and applies the appropriate `LIMIT` and `OFFSET` to your query
2. Counts the total number of matching rows and returns a `Pagination` value with all the information needed to render page controls

### View

In your view, add `pagination` to your view data type and call `renderPagination`:

```haskell
data IndexView = IndexView
    { users :: [User]
    , pagination :: Pagination
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Users</h1>
        <table>
            <tr>
                <th>Name</th>
                <th>Email</th>
            </tr>
            {forEach users renderUser}
        </table>
        {renderPagination pagination}
    |]
```

`renderPagination` renders a page navigation bar with page numbers, "Previous" and "Next" links, and an items-per-page selector. It automatically uses your application's CSS framework (Bootstrap by default). If there is only one page of results, the pagination controls are hidden.

## Custom Options

By default, `paginate` shows 50 items per page with a window size of 5 pages in the page selector. Use `paginateWithOptions` to customize these:

```haskell
action UsersAction = do
    (usersQuery, pagination) <- query @User
        |> orderBy #createdAt
        |> paginateWithOptions
            (defaultPaginationOptions
                |> set #maxItems 10
                |> set #windowSize 3
            )
    users <- usersQuery |> fetch
    render IndexView { .. }
```

- **`maxItems`**: Maximum number of items per page (default: `50`). The user can override this via the `maxItems` URL parameter, but it is capped at 200 to prevent abuse.
- **`windowSize`**: Number of pages shown before and after the current page in the page selector (default: `5`).

## Filtering

IHP provides built-in text filtering that works alongside pagination. Use `filterList` in your controller to filter results by a text field using case-insensitive `ILIKE` matching:

```haskell
action UsersAction = do
    (usersQuery, pagination) <- query @User
        |> orderBy #email
        |> paginate
    users <- usersQuery
        |> filterList #email
        |> fetch
    render IndexView { .. }
```

`filterList` reads the `filter` query parameter from the URL. If present, it adds a `WHERE email ILIKE '%searchterm%'` clause to the query.

In your view, use `renderFilter` to display a search box:

```haskell
instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="container">
            <div class="row justify-content-between">
                <div class="col-7">
                    <h1>Users</h1>
                </div>
                <div class="col-5">
                    {renderFilter "Search by email"}
                </div>
            </div>
        </div>
        <table>
            {forEach users renderUser}
        </table>
        {renderPagination pagination}
    |]
```

`renderFilter` renders a form with a text input and "Filter" / "Clear" buttons. The placeholder text passed as an argument is shown in the input field.

## Raw SQL Pagination

If you need to paginate a raw SQL query, use `paginatedSqlQuery`:

```haskell
action UsersAction = do
    (users, pagination) <- paginatedSqlQuery
        "SELECT id, name, email FROM users WHERE active = ?"
        (Only True)
    render IndexView { .. }
```

This wraps your query as a subquery to count total results and apply `LIMIT`/`OFFSET`. The result is a list of records and a `Pagination` value, just like `paginate`.

To customize options with raw SQL, use `paginatedSqlQueryWithOptions`:

```haskell
action UsersAction = do
    (users, pagination) <- paginatedSqlQueryWithOptions
        (defaultPaginationOptions |> set #maxItems 10)
        "SELECT id, name, email FROM users WHERE active = ?"
        (Only True)
    render IndexView { .. }
```

**Note:** When using `paginatedSqlQuery` with [AutoRefresh](auto-refresh.html), you need to call `trackTableRead` to let AutoRefresh know which tables your query accesses. Otherwise AutoRefresh will not watch those tables for changes.

## Helper Functions

The `IHP.Pagination.Helpers` module provides functions for working with `Pagination` values in your views:

### `getLastPage`

Returns the number of the last page:

```haskell
getLastPage pagination
-- Example: 10 (if there are 10 pages of results)
```

### `hasNextPage`

Returns `True` if there is a next page after the current one:

```haskell
hasNextPage pagination
-- True if currentPage < lastPage
```

### `hasPreviousPage`

Returns `True` if there is a previous page before the current one:

```haskell
hasPreviousPage pagination
-- True if currentPage > 1
```

These helpers are useful for building custom pagination controls or conditionally showing content based on the pagination state.

## Query Parameters

Pagination reads and writes the following URL query parameters automatically:

| Parameter | Description | Default |
|-----------|-------------|---------|
| `page` | The current page number | `1` |
| `maxItems` | Items per page (overrides the `maxItems` option, capped at 200) | `50` |
| `filter` | Text search term (used by `filterList`) | none |

For example, `/Users?page=3&maxItems=25` shows page 3 with 25 items per page.

## Code Generator

When generating a new controller with the IHP code generator, you can check the **Pagination** option. This will generate controller actions and views that include pagination out of the box.
