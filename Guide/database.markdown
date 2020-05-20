# Database

```toc
```

## Introduction

IHP provides a few basic functions to access the database. We try to provide a thin layer on top of Postgres SQL, to make it easy to do all the common tasks your web application usually does.

The only supported database platform is Postgres. Focusing on Postgres allows us to better integrate advanced postgres specific solutions into your application.

In development you do not need to set up anything to use postgres. The built-in development server automatically starts a Postgres instance to work with your application. The built-in development postgres servers is only listening on a unix socket and not available via TCP.

When the dev server is running, you can connect to it via `postgresql:///app?host=YOUR_PROJECT_DIRECTORY/build/db` with your favorite database tool. From the terminal you can also use `make psql` when inside the project directory, to open a postgres REPL connected to the development database. The web interface of the dev server also has a GUI-based database editor (like phpmyadmin) at [http://localhost:8001/ihp/ShowDatabase](http://localhost:8001/ihp/ShowDatabase).

Haskell data structures and types are generated automatically based on your database schema.

### Schema.sql

Once you have created your project, the first step is define a database schema. The database schema is basically just a SQL file with a lot of `CREATE TABLE ...` statements. You can find it at `Application/Schema.sql`.

In a new project this file will look pretty empty, like this:

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
```

The single line just activates the UUID extension for your database.

To define your database schema, add your `CREATE TABLE ...` statements to the `Schema.sql`. For a users table this can look like this:

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL
);
```

Haskell data structures and types are automatically generated from the `Schema.sql` file. They are re-generated on every file change of the `Schema.sql`. We use the well known `postgresql-simple` haskell library to connect to the database. 

### Schema Designer

Because the SQL syntax is sometimes hard to remember, the framework provides a GUI-based database editor called IHP Schema Designer. You can find the Schema Designer at `http://localhost:8001/ihp/Tables`.

![The Schema Designer in Visual Mode](images/database/schema-designer-visual.png)

Keep in mind, that the Schema Editor also only modifies the `Schema.sql`. This works by parsing the SQL DDL-statements, applying transformations on the AST and the compiling and writing it back to `Schema.sql`. When there is an syntax error in the `Schema.sql` file, the visual mode will be unavailable and you have to work with the code editor to fix the problem.

You can add tables, columns, foreing key constraints and enums. You can also edit these objects by right clicking them. New tables have a `id` column by default. Lots of opinionated short-cuts for rapid application development, like automatically offering to add foreign key constraints, are built-in.

![An example of using the context menu for editing a table](images/database/schema-designer-context-menu.png)

When the Visual Editor is not powerful enough, just switch back to the code editor. For convience, the Schema Designer also allows you to toggle to the Code Editor inside the web browser:

![The Schema Designer in Code Mode](images/database/schema-designer-code.png)

### Push to DB

Afer we have added a few data structures to our `Schema.sql`, our running Postgres database is still empty. This is because we still need to import our database schema into the database.

**In the Schema Designer:** Click on `Push to DB`:

![Push to DB Button](images/database/schema-designer-push-to-db.png)

**In the command line:** Run `make db`.

This will delete and re-create the current database and import the `Schema.sql`. After importing the Schema, it will also import the `Application/Fixtures.sql` which is used for prepopulating the empty database with some data. It's equivalent to running `psql < Schema.sql; psql < Fixtures.sql` inside an empty database.

When the dev server is started the first time, the `Schema.sql` and `Fixtures.sql` are automatically imported.

### Fixtures.sql

The `Fixtures.sql` includes a lot of `INSERT INTO` statements to prefill your database once the schema has been created.

You can manually add `INSERT INTO` statements to this file. You can also *migrate* your fixtures by just making the required changes to this sql file.

You can dump your current database state into the `Fixtures.sql` by running `make dumpdb`. This way you can regularly commit the database state to git, so other developers have the same data inside their local development database as you have.

## Haskell Bindings

### Model Context

In a pure functional programming language like haskell, we need to pass the database connection to all functions which need to access the database. We use a implicit parameter `?modelContext :: ModelContext` to pass around the database connection without always specifying it. The `ModelContext` data structure is basically just a wrapper around the actually database connection.

An implicit paramter is a parameter which is automatically passed to certain functions, it just needs to be available in the current scope.

This means that all functions which are running database queries, will need to be called from an function which has this implicit parameter in scope. A function doing something with the database, will always have a type signature specifing that it requires the `?modelContext` to be available, like this:

```haskell
myFunc :: (?modelContext :: ModelContext) => IO SomeResult
````

All controller actions already have `?modelContext` in scope and thus can run database queries. Other application entry-points, like e.g. Scripts, also have this in scope.

This also means, that when a function does not specify that it depends on the database connection in it's type signature (like `?modelContext :: ModelContext => ..`), you can be sure that it's not doing any database operations.

### Haskell Data Structures

For every table in the `Schema.sql` a coresponding data structure will be generated on the haskell side. For example given a table:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL
);
```

The generated haskell data structure for this table will look like this:

```haskell
data User = User
    { id :: Id User
    , firstname :: Text
    , lastname :: Text
    }
```

The `id` field type `Id User` is basically just a wrapper around `UUID` for type-safety reasons. All database field names are mapped from `under_score` to `camelCase` on the haskell side.

When a sql field can be `NULL`, the haskell field type will be contained in `Maybe`.

In the Schema Designer you can take a look at the generated haskell code by right-clicking the table and clicking `Show Generated Haskell Code`.

## Retrieving Records

### Querying Records

You can retrieve all records of a table using `query`

```haskell
do
    users <- query @User |> fetch
    forEach users \user -> do
        putStrLn (get #name user)
```

This will run a `SELECT * FROM users` query and put a list of `User` structures.

### Fetching a single record

When you have the id of a record, you can also use `fetch` to get it from the database:

```haskell
do
    let userId :: Id User = ...
    user <- fetch userId
    putStrLn (get #name user)
```

This will run the SQL query `SELECT * FROM users WHERE id = ... LIMIT 1`.

`fetch` knows only a single entity will be returned for the id, so instead of a list of users just a single user will be returned. In case the entity is not found, an exception is thrown. Use `fetchOrNothing` to get `Nothing` instead of an exception when no result is found

### Fetching a list of ids

When have you a list of ids of a single record type, you can also just `fetch` them:

```haskell
do
    let userIds :: [Id User] = ...
    users <- fetch userIds
```

This will run the SQL query `SELECT * FROM users WHERE id IN (...)`. The results in `users` have type `[User]`.


## Raw SQL Queries

Use the function `sqlQuery` to run a raw sql query.

```haskell
do
    result <- sqlQuery "SELECT * FROM projects WHERE id = ?" (Only id)
```

You might need to specify the expected result type, as type inference might not be able to guess it.

You can query any kind of information, not only records:

```haskell
do
    count :: Int <- sqlQuery "SELECT COUNT(*) FROM projects" []

    randomString :: Text <- sqlQuery "SELECT md5(random()::text)" []
```

## Create

### Creating a single record

To insert a record into the database, call `newRecord` to get an empty record value:

```haskell
do
    let user = newRecord @User
    -- user = User { id = 0000-0000-0000-0000, firstname = "", lastname = "" }
```

The `newRecord` function does not insert the record, it just returns a new empty data structure we can fill with values and then insert into the database.

We can use `set` to fill in attributes:

```haskell
do
    let user = newRecord @User
            |> set #firstname "Max"
            |> set #lastname "Mustermann"

    -- user = User { id = 0000-0000-0000-0000, firstname = "Max", lastname = "Mustermann" }
```

We use `createRecord` to insert the above record into the `users` table:

```haskell
do
    let user = newRecord @User
            |> set #firstname "Max"
            |> set #lastname "Mustermann"

    -- user = User { id = 0000-0000-0000-0000, firstname = "Max", lastname = "Mustermann" }

    insertedUser <- user |> createRecord
    -- This will run INSERT INTO users (id, firstname, lastname) VALUES (DEFAULT, "Max", "Mustermann");
    -- insertedUser = User { id = cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7, firstname = "Max", lastname = "Mustermann" }
```

This can also be rewritten in a more compact form:

```haskell
do
    user <- newRecord @User
            |> set #firstname "Max"
            |> set #lastname "Mustermann"
            |> createRecord
    -- user = User { id = "cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7", firstname = "Max", lastname = "Mustermann" }
```

### Creating many records

You can use `createMany` to insert multiple records with a single `INSERT` statement:

```haskell
do
    let usersToBeInserted [ newRecord @User, newRecord @User, ... ]
    users <- createMany usersToBeInserted
```

This will run:

```sql
INSERT INTO users (id, firstname, lastname)
    VALUES (DEFAULT, "", ""), (DEFAULT, "", "") , (DEFAULT, "", "") ... ;
```


## Update

The function `updateRecord` runs an `UPDATE` query for a specific record:

```haskell
do
    user <- fetch ("cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7" :: Id User)
    user
        |> set #lastname "Tester"
        |> updateRecord
```

This will set the lastname of user `cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7` to `Tester` and run and `UPDATE` query to persist that:

```sql
UPDATE users SET firstname = "Max", lastname = "Tester" WHERE id = "cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7"
```

## Delete

### Deleting a single record

Use `deleteRecord` to run a simple `DELETE` query:

```haskell
do
    user <- fetch ("cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7" :: Id User)
    deleteRecord user
```

This will execute:
```sql
DELETE FROM users WHERE id = "cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7"
```


### Deleting many records

Use `deleteRecords` to run a `DELETE` query for multiple records:

```haskell
do
    users :: [User] <- ...
    deleteRecords users
```

This will execute:
```sql
DELETE FROM users WHERE id IN (...)
```


## Query Builder

## Changing the Schema

## Relationships

## Advanced Queries With the Query Builder


