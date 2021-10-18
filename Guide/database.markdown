# Database

```toc

```

## Introduction

Haskell data structures and types are generated automatically based on your database schema.

IHP provides a few basic functions to access the database. On top of Postgres SQL, we try to provide a thin layer to make it easy to do all the common tasks your web application usually does.

The only supported database platform is Postgres. Focusing on Postgres allows us to better integrate advanced Postgres-specific solutions into your application.

In development, you do not need to set up anything to use Postgres. The built-in development server automatically starts a Postgres instance to work with your application. The built-in development Postgres server is only listening on a Unix socket and is not available via TCP.

### Connecting to DB via Terminal

When the development server is running, you can connect to it via `postgresql:///app?host=YOUR_PROJECT_DIRECTORY/build/db` with your favorite database tool. When inside the project directory you can also use `make psql` to open a Postgres REPL connected to the development database (named `app`), or start `psql` by pointing at the local sockets file `psql --host=/PATH/TO/PROJECT/DIRECTORY/build/db app`. The web interface of the development server also has a GUI-based database editor (like phpmyadmin) at [http://localhost:8001/ShowDatabase](http://localhost:8001/ShowDatabase).

### Connecting to DB via UI

When the development server is running, you can use your favorite UI tool (e.g. [TablePlus](https://tableplus.com/)) that allows connecting to Postgress. To do that you would need the following credentials:

Database Host: This is the application root + "/build/db". Use this command on terminal form the root of you app and copy the output:
```
echo `pwd`/build/db
```

Database username: This is the current user you run the terminal with. Run `whoami` command to get that name.
Database name: `app`

### Schema.sql

Once you have created your project, the first step is to define a database schema. The database schema is a SQL file with a lot of `CREATE TABLE ...` statements. You can find it at `Application/Schema.sql`.

In a new project, this file will be empty. The [`uuid-ossp`](https://www.postgresql.org/docs/current/uuid-ossp.html) extension is automatically enabled for the database by IHP.

To define your database schema add your `CREATE TABLE ...` statements to the `Schema.sql`. For a users table this can look like this:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL
);
```

Haskell data structures and types are automatically generated from the `Schema.sql` file. They are re-generated on every file change of the `Schema.sql`. We use the well-known [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple) Haskell library to connect to the database.

### Schema Designer

Because the SQL syntax is sometimes hard to remember, the framework provides a GUI-based database editor called IHP Schema Designer. You can find the Schema Designer at `http://localhost:8001/Tables`.

![The Schema Designer in Visual Mode](images/database/schema-designer-visual.png)

Keep in mind that the Schema Editor also only modifies the `Schema.sql`. This works by parsing the SQL DDL-statements and applying transformations on the AST, compiling and writing it back to `Schema.sql`. When there is a syntax error in the `Schema.sql` file the visual mode will be unavailable and you have to work with the code editor to fix the problem.

You can add tables, columns, foreign key constraints, and enums. You can also edit these objects by right-clicking them. New tables have an `id` column by default. Lots of opinionated short-cuts for rapid application development like automatically offering to add foreign key constraints are built-in.

![An example of using the context menu for editing a table](images/database/schema-designer-context-menu.png)

When the Visual Editor is not powerful enough, just switch back to the code editor. For convenience, the Schema Designer also allows you to toggle to the Code Editor inside the web browser:

![The Schema Designer in Code Mode](images/database/schema-designer-code.png)

### Push to DB

After we have added a few data structures to our `Schema.sql`, our running Postgres database is still empty. This is because we still need to import our database schema into the database.

**In the Schema Designer:** Click on `Push to DB`:

![Push to DB Button](images/database/schema-designer-push-to-db.png)

**In the command line:** Run `make db` while the server is running.

This will delete and re-create the current database and import the `Schema.sql`. After importing the Schema, it will also import the `Application/Fixtures.sql` which is used for pre-populating the empty database with some data. It's equivalent to running `psql < Schema.sql; psql < Fixtures.sql` inside an empty database.

When the development server is started the first time, the `Schema.sql` and `Fixtures.sql` are automatically imported.

### Fixtures.sql

The `Fixtures.sql` includes a lot of `INSERT INTO` statements to pre-fill your database once the schema has been created.

You can manually add `INSERT INTO` statements to this file. You can also _migrate_ your fixtures by just making the required changes to this SQL file.

You can dump your current database state into the `Fixtures.sql` by running `make dumpdb`. This way you can regularly commit the database state to git, so other developers have the same data inside their local development database as you have.

### Update DB

You can also update the database while keeping its contents.

**In the Schema Designer:** Click on `Update DB`:

![Push to DB Button](images/database/schema-designer-push-to-db.png)

**In the command line:** Run `make dumpdb` and after that `make db`.

When dumping the database into the `Fixtures.sql` first and then rebuilding the database with the dump, the contents will be kept when changing the schema.

## Haskell Bindings

### Model Context

In a pure functional programming language like Haskell, we need to pass the database connection to all functions which need to access the database. We use an implicit parameter `?modelContext :: ModelContext` to pass around the database connection without always specifying it. The [`ModelContext`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:ModelContext) data structure is basically just a wrapper around the actual database connection.

An implicit parameter is a parameter which is automatically passed to certain functions, it just needs to be available in the current scope.

This means that all functions which are running database queries will need to be called from a function which has this implicit parameter in scope. A function doing something with the database, will always have a type signature specifying that it requires the `?modelContext` to be available, like this:

```haskell
myFunc :: (?modelContext :: ModelContext) => IO SomeResult
```

All controller actions already have `?modelContext` in scope and thus can run database queries. Other application entry-points, like e.g. Scripts, also have this in scope.

This also means, that when a function does not specify that it depends on the database connection in its type signature (like `?modelContext :: ModelContext => ..`), you can be sure that it's not doing any database operations.

### Haskell Data Structures

For every table in the `Schema.sql` a corresponding data structure will be generated on the Haskell side. For example, given a table:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    firstname TEXT NOT NULL,
    lastname TEXT NOT NULL
);
```

The generated Haskell data structure for this table will look like this:

```haskell
data User = User
    { id :: Id User
    , firstname :: Text
    , lastname :: Text
    }
```

The `id` field type `Id User` is basically just a wrapper around `UUID` for type-safety reasons. All database field names are mapped from `under_score` to `camelCase` on the Haskell side.

When a SQL field can be `NULL`, the Haskell field type will be contained in `Maybe`.

In the Schema Designer, you can take a look at the generated Haskell code by right-clicking the table and clicking `Show Generated Haskell Code`.

## Retrieving Records

### Querying Records

You can retrieve all records of a table using [`query`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:query)

```haskell
do
    users <- query @User |> fetch
    forEach users \user -> do
        putStrLn (get #name user)
```

This will run a `SELECT * FROM users` query and put a list of `User` structures.

### Fetching a single record

When you have the id of a record, you can also use [`fetch`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetch) to get it from the database:

```haskell
do
    let userId :: Id User = ...
    user <- fetch userId
    putStrLn (get #name user)
```

This will run the SQL query `SELECT * FROM users WHERE id = ... LIMIT 1`.

[`fetch`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetch) knows a single entity will be returned for the id, so instead of a list of users, a single user will be returned. In case the entity is not found, an exception is thrown. Use [`fetchOrNothing`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetchOneOrNothing) to get `Nothing` instead of an exception when no result is found

### Fetching a list of ids

When have you a list of ids of a single record type, you can also just [`fetch`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetch) them:

```haskell
do
    let userIds :: [Id User] = ...
    users <- fetch userIds
```

This will run the SQL query `SELECT * FROM users WHERE id IN (...)`. The results in `users` have type `[User]`.

### Fetching a `Maybe (Id record)`

Sometimes you have an optional id field, like e.g. when having a database schema like this:

```sql
CREATE TABLE tasks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    description TEXT,
    assigned_user_id UUID
);
```

In this case the field `assigned_user_id` can be null. In our action we want to fetch user when it's not null, and return `Nothing` otherwise:

```haskell
action ShowTask { taskId } = do
    task <- fetch taskId
    assignedUser <- case get #assignedUserId task of
            Just userId -> do
                user <- fetch userId
                pure (Just user)
            Nothing -> pure Nothing
```

This contains a lot of boilerplate for wrapping and unwrapping the [`Maybe`](https://ihp.digitallyinduced.com/api-docs/IHP-Prelude.html#t:Maybe) value. Therefore you can just call [`fetchOneOrNothing`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetchOneOrNothing) directly on the `Maybe (Id User)` value:

```haskell
action ShowTask { taskId } = do
    task <- fetch taskId
    assignedUser <- fetchOneOrNothing (get #assignedUserId task)
```

### Fetching `n` records (LIMIT)

Use [`limit`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:limit) to query only up to `n` records from a table:

```haskell
do
    users <- query @User
        |> orderBy #firstname
        |> limit 10
        |> fetch
```

This will run a `SELECT * FROM users ORDER BY firstname LIMIT 10` query and will return the first 10 users ordered by their `firstname`.

When you are only interested in the first result you can also use [`fetchOne`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetchOne) as a shortcut for [`|> limit 1`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:limit):

```haskell
do
    firstUser <- query @User
        |> orderBy #firstname
        |> fetchOne
```

### Skipping `n` records (OFFSET)

Use [`offset`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:offset) to skip `n` records from a table:

```haskell
do
    users <- query @User
        |> orderBy #firstname
        |> offset 10
        |> fetch
```

This is most often used together with [`limit`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:limit) to implement paging.

### Counting records (COUNT queries)

You can use [`fetchCount`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetchCount) instead of [`fetch`](https://ihp.digitallyinduced.com/api-docs/IHP-Fetch.html#v:fetch) to get the count of records matching the query:

```haskell
do
    activeUsersCount :: Int <- query @User
        |> filterWhere (#isActive, True)
        |> fetchCount

    -- SELECT COUNT(*) FROM users WHERE is_active = 1
```

### Fetching distinct records

Use [`distinct`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:distinct) to fetch distinct records.

```haskell
do
    posts <- query @Post
        |> distinct
        |> fetch
```

Or [`distinctOn #tableField`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:distinctOn) to fetch distinct records based on the `#tableField` value.

```haskell
do
    users <- query @User
        |> distinctOn #fullName
        |> fetch
```

## Raw SQL Queries

The IHP query builder is designed to be able to easily express many basic sql queries. When your application is growing you will typically hit a point where a complex SQL query cannot be easily expressed with the IHP query builder. In that case it's recommended to use handwritten SQL to access your data.

Use the function [`sqlQuery`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:sqlQuery) to run a raw SQL query:

```haskell
do
    result <- sqlQuery "SELECT * FROM projects WHERE id = ?" (Only id)

    -- Query with WHERE id IN
    result <- sqlQuery "SELECT * FROM projects WHERE id IN ?" (Only (In [id]))

    -- Get a lists of posts with their Comment count
    let postIds :: [Id Post] = ["1c3a81ff-55ca-42a8-82e0-31d04f642e53"]
    commentsCount :: [(Id Post, Int)] <- sqlQuery "SELECT post_id, count(*) FROM comments WHERE post_id IN ? GROUP BY post_id" (Only (In postIds))
```

You might need to specify the expected result type, as type inference might not be able to guess it.

```haskell
do
    result :: [Project] <- sqlQuery "SELECT * FROM projects WHERE id = ?" (Only id)
```

If you would like to have your query dynamically built with an argument you could:

```haskell
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Types as PG

do
    -- Get all Projects
    let table :: Text = "projects"
    -- Use PG.Identifier to prevent SQL injection
    result :: [Project] <- sqlQuery "SELECT * FROM ?" [PG.Identifier table]
```

### Scalar Results

The [`sqlQuery`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:sqlQuery) function always returns a list of rows as the result. When the result of your query is a single value (such as an integer or string) use [`sqlQueryScalar`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:sqlQueryScalar):

```haskell
do
    count :: Int <- sqlQueryScalar "SELECT COUNT(*) FROM projects" ()

    randomString :: Text <- sqlQueryScalar "SELECT md5(random()::text)" ()
```

### Dealing With Complex Query Results

Let's say you're querying posts and a count of comments on each post:


```haskell
do
    result :: [Post] <- sqlQuery "SELECT posts.id, posts.title, (SELECT COUNT(*) FROM comments WHERE comments.post_id = posts.id) AS comments_count FROM posts" ()
```

This will fail at runtime because the result set cannot be decoded as expected. The result has the columns `id`, `title` and `comments_count` but a Post record consists of `id`, `title`, `body`.

The solution here is to write our own data type and mapping code:

```haskell
module Application.PostsQuery where

import IHP.Prelude
import IHP.ModelSupport
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data PostWithCommentsCount = PostWithCommentsCount
    { id :: Id Post
    , title :: Text
    , commentsCount :: Int
    }
    deriving (Eq, Show)

instance FromRow PostWithCommentsCount where
    fromRow =
        PostWithCommentsCount
            <$> field
            <*> field
            <*> field

fetchPostsWithCommentsCount :: (?modelContext :: ModelContext) => IO [PostWithCommentsCount]
fetchPostsWithCommentsCount = do
    trackTableRead "posts" -- This is needed when using auto refresh, so auto refresh knows that your action is accessing the posts table
    sqlQuery "SELECT posts.id, posts.title, (SELECT COUNT(*) FROM comments WHERE comments.post_id = posts.id) AS comments_count FROM posts" ()
```

You can now fetch posts with their comments count like this:

```haskell
import Application.PostsQuery as PostsQuery

action MyAction = do
    postsWithCommentsCount <- PostsQuery.fetchPostsWithCommentsCount

    render MyView { ..}
```

#### Complex Query Results: Real-world Example

Here's a real world example of dealing with complex query results:

```haskell
module Application.People.Query (
    fetchActiveWorkers,
    Row (..),
) where

import Application.People.Person (botGoesBy)
import "string-interpolate" Data.String.Interpolate (i)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import qualified Generated.Types as Types
import IHP.ModelSupport
import IHP.Prelude

data Row = Row
    { id :: !(Id Types.Person)
    , firstName :: !Text
    , lastName :: !Text
    , goesBy :: !Text
    , sendMessageActionState :: !(Maybe Text)
    }
    deriving (Eq, Show)

instance FromRow Row where
    fromRow =
        Row
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field

fetchActiveWorkers :: (?modelContext :: ModelContext) => IO [Row]
fetchActiveWorkers = do
    trackTableRead "people"
    trackTableRead "worker_settings"
    trackTableRead "action_run_states"
    trackTableRead "send_message_actions"
    sqlQuery query ()

query :: Query
query =
    [i|
        select
            people.id,
            people.first_name,
            people.last_name,
            people.goes_by,
            max(action_run_states.state) send_message_action_state
        from
            people
            inner join
                worker_settings on (worker_settings.person_id = people.id)
            inner join
                phone_contacts on (phone_contacts.person_id = people.id)
            left join
                send_message_actions on (send_message_actions.to_id = phone_contacts.phone_number_id)
            left join
                action_run_states on (
                    action_run_states.id = send_message_actions.action_run_state_id
                    and (action_run_states.state = 'not_started' or action_run_states.state = 'suspended')
                )
        where
            people.goes_by <> '#{botGoesBy}'
            and worker_settings.is_active
        group by
            people.id
        order by
            people.last_name,
            people.first_name;
    |]
```

`Row` is the data type used to hold the result of the query. The use of [`trackTableRead`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:trackTableRead) enables the query to play nicely with Auto Refresh.

## Create

### Creating a single record

To insert a record into the database, call [`newRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:Record) to get an empty record value:

```haskell
do
    let user = newRecord @User
    -- user = User { id = 0000-0000-0000-0000, firstname = "", lastname = "" }
```

The [`newRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:Record) function does not insert the record, it just returns a new empty data structure we can fill with values and then insert into the database.

We can use [`set`](https://ihp.digitallyinduced.com/api-docs/IHP-HaskellSupport.html#v:set) to fill in attributes:

```haskell
do
    let user = newRecord @User
            |> set #firstname "Max"
            |> set #lastname "Mustermann"

    -- user = User { id = 0000-0000-0000-0000, firstname = "Max", lastname = "Mustermann" }
```

We use [`createRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-HaskellSupport.html#v:set) to insert the above record into the `users` table:

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

You can use [`createMany`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:createMany) to insert multiple records with a single `INSERT` statement:

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

The function [`updateRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:CanUpdate) runs an `UPDATE` query for a specific record:

```haskell
do
    user <- fetch ("cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7" :: Id User)
    user
        |> set #lastname "Tester"
        |> updateRecord
```

This will set the `lastname` of user `cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7` to `Tester` and run an `UPDATE` query to persist that:

```sql
UPDATE users SET firstname = firstname, lastname = "Tester" WHERE id = "cf633b17-c409-4df5-a2fc-8c3b3d6c2ea7"
```

The `UPDATE` query will only update columns that have been changed using `|> set #someField someValue` on the record.

## Delete

### Deleting a single record

Use [`deleteRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:deleteRecord) to run a simple `DELETE` query:

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

Use [`deleteRecords`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:deleteRecords) to run a `DELETE` query for multiple records:

```haskell
do
    users :: [User] <- ...
    deleteRecords users
```

This will execute:

```sql
DELETE FROM users WHERE id IN (...)
```

### Deleting all records

Use [`deleteAll`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:deleteAll) to run a `DELETE` query for all rows in a table:

```haskell
do
    deleteAll @User
```

This will execute:

```sql
DELETE FROM users
```

## Enums

It's possible to define and use custom enum types with IHP. An enum can be created using the Schema Designer. The process is pretty much the same as when creating a table.

### Adding enums via the Schema Designer

Open the Schema Designer, right-click into the `Objects` Pane, and then select `Add Enum`:

![](images/database/schema-designer-context-menu.png)

You have to give a name to your enum type. The name should be in plural form, like with table names. E.g. we could name our enum `colors`.

Next, add the enum values by right-clicking into the `Values` pane and click on `Add Value`. Here we could add values such as `red`, `blue` and `yellow`.

### Adding enums via SQL

Instead of using the Schema Designer, you can also just add the required SQL statement manually into `Application/Schema.sql`:

```sql
CREATE TYPE colors AS ENUM ('blue', 'red', 'yellow');
```

### Using enums in the Haskell Code

The above `colors` example will allow us to access the enum like this:

```haskell
let blue :: Colors = Blue
let red :: Colors = Red
let yellow :: Colors = Yellow
```

You can use the enum as a field type for another record. E.g. we can have `posts` table and there give each post a color:

```sql
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    body TEXT NOT NULL,
    color colors
);
```

You can use `fill` even with custom enums:

```haskell
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> fill @["body", "color"]
            |> ifValid \case
                Left post -> render NewView { .. }
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction
```

In your views, use [`inputValue`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#t:InputValue) to get a textual representation for your enum which works with [`fill`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:fill):

```html
[hsx|
<input type="text" value="{inputValue" Blue} />
|]
```

## Database Updates

The _Update DB_ operation is three steps:

1. Data is read from the database and stored in `Fixtures.sql`
2. The database is deleted and the schema in `Schema.sql` created
3. The data in `Fixtures.sql` is (re-)inserted.

The small arrow on the Update DB button shows a menu where it is possible to just run _Save DB to Fixtures_ (step 1) or _Push to DB_ (steps 2 + 3).

## Making Changes to the Database

The main purpose of the below steps are for keeping the data rows from `Application/Fixtures.sql` between updates. If you are not concerned with keeping these rows when refactoring, feel free to skip this section.

The main reason for the steps outlined below is that changes to the database schema are written to `Application/Schema.sql` only. The actual database is not altered. This means the actual schema and `Schema.sql` will be out of sync.

When the schemas are out of sync, the INSERT statements in `Fixtures.sql` will fail. If this happens, and you attempt to update, the target table will be empty after the update. Try again, and the empty table is read from the database and the data in `Fixtures.sql` is gone.

If you feel this is all a bit less streamlined compared to the rest of the development experience, you are correct. We will work on improving handling changes to the database.

### Adding a Column

#### Nullable or with Default

This is always OK. The existing rows can be re-inserted from your `Fixtures.sql` without errors. After another update cycle, `Fixtures.sql` will also contain the new column.

#### Non-Nullable or Without Default

It's best to do this in two steps. First, follow the above. After updating the DB, fill the column with data and remove the nullable or default properties.

Data can be updated by manually editing the table in the data view or by running UPDATE statements in the custom query field below the data view.

### Renaming a Column

When you are renaming a column, the development process of using `Update DB` will not work. This is because `Update DB` will save the old database state into the `Fixtures.sql`. There it still references the old column names. It will then fail on the next update.

1. Rename your column `col_a` to `col_b` in the Schema Designer
2. Rename the column in the database by executing `ALTER TABLE tablename RENAME COLUMN col_a TO col_b` in the custom query field below the data view.

### Deleting a Column

Similarly as for renaming, deleting a column currently won't work automatically either.

1. Delete your column in the Schema Designer
2. Delete the column from the database by executing `ALTER TABLE tablename DROP COLUMN colname`

### Alternate Method

There's always more than one way. This is another.

1. Make changes in the Schema Designer
2. Click `Save DB to Fixtures` in the Schema Designer (Use the arrow next to the `Update DB` button to see this option)
3. Edit `Fixtures.sql` to your heart's content.
4. Click `Push to DB` in the Schema Designer (Use the arrow next to the `Update DB` button to see this option)

### Migrations In Production

IHP currently has no built-in migration system yet. We're still experimenting with a great way to solve this. Until then, the recommended approach used by digitally induced is to manually migrate your database using DDL statements as shown above.


## Supported Database Types

IHP currently has support for the following postgres column types:

- UUID
- Text
- VARCHAR(..), CHARACTER VARYING(..)
- TIMESTAMP WITHOUT TIMEZONE, TIMESTAMP
- TIMESTAMP WITH TIMEZONE, TIMESTAMPZ
- BIGINT, INT8
- SMALLINT, INT2
- INTEGER, INT4, INT
- BOOLEAN, BOOL
- REAL, FLOAT4
- DOUBLE PRECISION, FLOAT8
- POINT
- DATE
- BYTEA
- TIME
- NUMERIC, NUMERIC(..)
- CHAR(..), CHARACTER(..)
- SERIAL
- BIGSERIAL
- JSONB
- INET (Only IP addresses, CIDR not supported yet)
- TSVECTOR
- Arrays of all the above types
- Custom types, usually enums

## Transactions

You can use the [`withTransaction`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:withTransaction) function to wrap your database operations in a postgres database transaction:

```haskell
withTransaction do
   company <- newRecord @Company |> createRecord

   user <- newRecord @User
       |> set #companyId (get #id company)
       |> createRecord

   company <- company
       |> setJust #ownerId (get #id user)
       |> updateRecord
```

In this example, when the creation of the User fails, the creation of the company will be rolled back. So that no
incomplete data is left in the database when there's an error.

The [`withTransaction`](https://ihp.digitallyinduced.com/api-docs/IHP-ModelSupport.html#v:withTransaction) function will automatically commit after it succesfully executed the passed do-block. When any exception is thrown, it will automatically rollback.

### Common Pitfalls

Keep in mind that some IHP functions like [`redirectTo`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Redirect.html#v:redirectTo) or [`render`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Render.html#v:render) throw a [`ResponseException`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:ResponseException). So code like below will not work as expected:

```haskell
action CreateUserAction = do
    withTransaction do
        user <- newRecord @User |> createRecord
        redirectTo NewSessionAction
```

The [`redirectTo`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Redirect.html#v:redirectTo) throws a [`ResponseException`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#t:ResponseException) and will cause a rollback. This code should be structured like this:

```haskell
action CreateUserAction = do
    user <- withTransaction do
        newRecord @User |> createRecord

    redirectTo NewSessionAction
```
