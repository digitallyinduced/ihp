# Getting Started With Turbo Haskell
Turbo Haskell is a full stack framework focused on rapid application development while striving for robust code quality.

This guide covers everything you need to ship software with interactive lambda. 

##### Feature Overview

- **Fully managed dev environment:** Works on any machine because all dependencies (even PostgreSQL) are managed using the nix package manager.
- **Bootstrap 4 Out of the box:** The default layout already integrates bootstrap 4.
- **Auto live reload using virtual dom in dev mode:** Each code change changes your local page to refresh. The refresh uses a diff based patch to avoid resetting the page state.
- **Build robust applications:** With the power of haskell your application are going to be a lot more robust. Pretty much no runtime errors in production.
- **Fast dev enviroment:** While we use a compiled language, the built-in dev server automatically reloads your code changes using the fastest way possible. Usually changes are reflected in less than 100ms (alot faster than your average webpack setup).
- **Very fast production environment:** Production response times around 30ms. Using [instant click](http://instantclick.io/) it can be faster than your average SPA.


```toc
```



### 1. Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application

For example postgresql and the haskell compiler are both dependencies of your app. as well as all the haskell or javascript packages you want to use. we use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed.

##### MacOS
Install nix with ...
##### Linux
TODO
##### Windows
Sorry, we don't support windows yet.

### 2. Installing Interactive Lambda

You can now install interactive lambda by running:

```bash
$ nix-env -f <(curl https://turbohaskell.digitallyinduced.com/turbohaskell-new.nix) -i turbohaskell-new
```


# Creating Your First Project
### 1. Project Setup

This guide will lead you to create a small blog application. To set up the project, open a terminal and type:

```bash
$ turbohaskell-new blog
```

The new `blog` directory now contains a couple of auto-generated files and directories that make up your app.

Here is a short overview of the whole structure:

| File or Directory             | Purpose                                                                     |
|-------------------------------|-----------------------------------------------------------------------------|
| Config/                       |                                                                             |
| Config/Config.hs              | Configuration for the framework and your application                        |
| Config/nix/nixpkgs-config.nix | Configuration for the nix package manager                                   |
| Config/nix/haskell-packages/  | Custom haskell dependencies can be placed here                              |
| src/                          | The main source code for your app                                           |
| src/Model/Schema.hs           | Models and database tables are defined here                                 |
| src/Apps/Web/Controller       | App controllers                                                             |
| src/Apps/Web/View/            | Html template files                                                         |
| static/                       | Images, css and javascript files                                            |
| .ghci                         | Default config file for the haskell interpreter                             |
| .gitignore                    | List of files to be ignored by git                                          |
| App.cabal, Setup.hs           | Config for the cabal package manager (TODO: maybe move to Config/App.cabal) |
| default.nix                   | Declares your app dependencies (like package.json or composer.json)         |
| Makefile                      | Default config file for the make build system                               |

### 2. Hello, World!

You now already have a working haskell app ready to be started
You can start the development server by running this in the `Blog` directory:

```bash
$ make
```


Your app is now running at [http://localhost:8000](http://localhost:8000).
The server can be stopped by pressing CTRL+C.


### 3. Data Structures & PostgreSQL

##### Schema Modeling

For our blog we're going to deal with posts. A post has a title and a body. As we are dealing with a database we also need an id. We are using UUIDs instead of the typical numerics ids for this.

A `posts` table in a PostgreSQL database could loke like this:


| id :: UUID                           | title :: Text                                          | body :: Text                                                                                                       |
|--------------------------------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| 8d040c2d-0199-4695-ac13-c301970cff1d | My Experience With Nix On OS X                         | Some time ago, I’ve moved this jekyll-based blog ...                                                               |
| b739bc7c-5ed1-43f4-944c-385aea80f182 | Deploying Private GitHub Repositories To NixOS Servers | In a previous post I’ve already shared a way to deploy private git repositories to a NixOS based server. While ... |


To work with posts in our application, we now have to declare this data schema.
Open `src/Model/Schema.hs` and add the following code:


```haskell
module Model.Schema where
import ClassyPrelude (Maybe (..), (<>), Bool (..))
import TurboHaskell.SchemaSupport

database = [
    table "posts"
        + field "id" primaryKey
        + field "title" text
        + field "body" text
    ]
```

        
##### Loading the Schema

Next we need to create the `posts` table in our local postgresql database.
Don't worry, the local development postgresql server is already running. The dev server has conveniently already started it.

Take a look at `src/Model/Schema.sql`. The dev server has auto-generated a `CREATE TABLE`-statement for us:

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Please don't make any modifications to this file as it's auto generated. Use src/Model/Schema.hs to change the schema
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL
);
```

We just need to load this sql statement into our database. Open your terminal and run:

```bash
$ make db
```

The `posts` table has been created now. Let's quickly connect to our database and see that everything is correct:

```bash
$ make psql

psql (9.6.12)
Type "help" for help.

-- Let's do a query to check that the table is there

app=# select * from posts;

 id | title | body
----+-------+------
(0 rows)

-- Look's alright! :)



-- We can quit the postgresql console by typing \q

app=# \q
```

Now our database is ready to be consumed by our app.

##### Record Types

By specificing the above schema, the framework automatically provides several types for us. Here is a short overview:

|        Type | `Post' id title body`                                                      |
|------------:|----------------------------------------------------------------------------|
|  Definition | `data Post' id title body = Post {id :: id, title :: title, body :: body}` |
| Description | A helper type used by all the "real" types. All the content is variable    |
| Example     | `Post { id = (), title = "Hello", body = () } :: Post' () Text ()`         |


|        Type | `Post`                                                                                               |
|-------------|------------------------------------------------------------------------------------------------------|
|  Definition | `type Post = Post' (Id Post) Text Text`                                    |
| Description | A Post record from the database                                                                      |
| Example     | `Post { id = cfefdc6c-c097-414c-91c0-cbc9a79fbe4f, title = "Hello World", body = "Some body text" } :: Post` |


|        Type | `NewPost`                                                                                  |
|-------------|--------------------------------------------------------------------------------------------|
|  Definition | `type NewPost = Post' (FieldWithDefault (Id Post)) Text Text` |
| Description | Post which is not yet saved to the database                                                |
| Example     | `Post { id = Default, title = "My new Post", body = "Hello World" } :: NewPost`                       |

  

|        Type | `Id Post`                                                               |
|-------------|-------------------------------------------------------------------------|
|  Definition | `newtype Id Post = Id UUID`                                             |
| Description | Type for the `id` field                                                 |
| Example     | `pack (read "5a8d1be2-33e3-4d3f-9812-b16576fe6a38" :: UUID) :: Id Post` |

### 4. Apps, Controllers, Views

Interactive Lambda uses controllers to deal with incoming requests. We can use the built-in code generators to generate an empty controller for our posts.

A controller belongs to an application. Your whole project can consistent of multiple sub applications. Typically your production app will need e.g. an admin backend application next to the default web application.

First we need to go to the Web app directory:

```bash
$ cd src/Apps/Web/
```

Now we can run the code generator:

```bash
$ gen/controller Posts

+ src/Apps/Web/Controller/Posts.hs
* src/Apps/Web/Routes.hs
* src/Apps/Web/Types.hs
* src/Apps/Web/App.hs (import)
* src/Apps/Web/App.hs (import)
+ src/Apps/Web/View/Posts/Show.hs
+ src/Apps/Web/View/Posts/New.hs
+ src/Apps/Web/View/Posts/Edit.hs
+ src/Apps/Web/View/Posts/Index.hs
``` 

You can see that lot's of files have been created and updated.

Open your browser at [http://localhost:8000/](http://localhost:8000/) to try out the new controller. The generator did all the initial work we need to get our usual CRUD actions going.

##### New Types

Let's first take a closer look at the changes in `src/Apps/Web/Types.hs`. Here a new data structures was created:


```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Generic, Data)
```

We have one constructor for each possible action. Here you can see a short description for all the constructors:

| Action                                 | Request                    | Description                        |
|----------------------------------------|----------------------------|------------------------------------|
| `PostsAction`                          | `GET /Posts`               | Lists all posts                    |
| `NewPostAction`                        | `GET /Posts/new`           | Displays the form to create a post |
| `ShowPostAction { postId = someId }`   | `GET /Posts/{someId}`      | Shows the posts with id $someId    |
| `CreatePostAction`                     | `POST /Posts`              | Endpoint to create a Post          |
| `EditPostAction { postId = someId }`   | `GET /Posts/{someId}/edit` | Displays the form to edit a post   |
| `UpdatePostAction { postId = someId }` | `POST /Posts/{someId}`     | Endpoint to submit a post update   |
| `DeletePostAction { postId = someId }` | `DELETE /Posts/{someId}`   | Deletes the post                   |


A request like "Show me the post with id `e57cfb85-ad55-4d5c-b3b6-3affed9c662c`" can be represented like `ShowPostAction { postId = e57cfb85-ad55-4d5c-b3b6-3affed9c662c }`.

The type `Id Post` is just a UUID, but wrapped within a newtype (`newtype Id model = Id UUID`).


##### Controller Implementation

The actual code running, when an action is executed, is defined in `src/Apps/Web/Controller/Posts.hs`. Let's take a look, step by step.

###### Imports

```haskell
module Apps.Web.Controller.Posts where

import Apps.Web.Controller.Prelude
import Apps.Web.View.Posts.Index
import Apps.Web.View.Posts.New
import Apps.Web.View.Posts.Edit
import Apps.Web.View.Posts.Show
```

In the header we just see some imports. Controllers always import a special `Apps.Web.Controller.Prelude` module. It provides e.g. controller helpers and also the framework specific functions we will see below. The controller also imports all its views. Views are also just "normal" haskell modules.

###### ChangeSet

```haskell
type instance ChangeSet Post = ["title", "body"]
type instance ChangeSet NewPost = ChangeSet Post
```

Here we can see the list of fields which can be modified or set by the controller. Sometimes you don't want to allow specific fields to be changed by a controller (e.g. a `createdAt` field should not be set from the user).

The second line specifies that when creating a new post (`NewPost`), the same attributes are allowed as when updating an existing post.

###### Instance


```haskell
instance Controller PostsController ControllerContext where
```

The controller logic is specified by implementing an instance of the `Controller` type-class. Don't worry about the `ControllerContext` parameter, we will get back to that later on.

###### Index Action

This is where the interesting part begins. As we will see below, the controller implementation is just an `action` function, pattern mattching over our `data PostsController` structure we defined in `src/Apps/Web/Types.hs`.

```haskell
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

This is the index action. It's called when opening `/Posts`. First it fetches all the posts from the database and then passes it along to the view. The `IndexView { .. }` is just shorthand for `IndexView { posts = posts }`.

###### New Action

```haskell
    action NewPostAction = do
        let post = newRecord
        render NewView { .. }
```

This is our endpoint for `/Posts/new`. It just creates an empty new post and then passes it to the `NewView`. The `newRecord` is giving us an empty `Post` model. It's equivalent to manually writing `Post { id = Default, title = "", body = "" }`.

###### Show Action

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```
This is our show action at `/Posts/{postId}`. Here we pattern match on the `postId` field of `ShowPostAction` to get post id of the given request. Then we just call `fetch` on that `postId` which gives us the specific `Post` record. Then we just pass that post to the view.

###### Edit Action

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }
```
Our `/Posts/{postId}/edit` action. It's pretty much the same as in the `action ShowPostAction`, just with a different view.


###### Update Action

```haskell
    action UpdatePostAction { postId } = do
        post <- fetch postId
        fromParams' post >>= \case
            Left post -> render EditView { .. }
            Right post -> do
                post <- updateRecord post
                setSuccessMessage "Post updated"
                redirectTo EditPostAction { .. }
```

This action deals with update requests for a specific post. As usual we pattern match on the `postId` and fetch it. 

The interesting part is `fromParams' post`. The `fromParams'` function reads in the updated fields from the request and sets the value on the passed `post` record. It only passes the fields specified by `ChangeLog Post`, so only `title` and `body`. It also checks for validation errors after setting the attributes (we will learn more about this later).

`fromParams' post` returns `Either Post Post`. `Left post` means that e.g. the `title` or `body` did not pass validation. `Right post` means that all parameters could be set on `post` without any errors. 

In the error case (`Left post ->`) we just re-render the `EditView`. The `EditView` then tells the user about alidation errors.

In the success case (`Right post ->`) we save the updated post to the database (with `updateRecord`). Then we set a success message and redirect the user back to the edit view.


```haskell
    action CreatePostAction = do
        fromParams @NewPost >>= \case
            Left post -> render NewView { .. } 
            Right post -> do
                post <- createRecord post
                setSuccessMessage "Post created"
                redirectTo PostsAction
```

Our create action, dealing with `POST /Posts` requests.

It's pretty much like the update action. The `fromParams @NewPost` is just shorthand for `fromParams' (newRecord @NewPost)`.

```haskell
    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction
```

The last action is dealing with `DELETE /Posts/{postId}` requests. It's pretty much like the other actions, we just call `deleteRecord` here.

##### Routes

The router is configured in `src/Apps/Web/Routes.hs`. The generator just places a single line there:

```haskell
instance RestfulController PostsController
```

This empty instance magically sets up the routing as you would have expected from a RESTful controller.

##### Views

We should also quickly take a look at our views.

Let first look at the show view in `src/Apps/Web/View/Posts/Show.hs`:

```haskell
module Apps.Web.View.Posts.Show where
import Apps.Web.View.Prelude

data ShowView = ShowView { post :: Post }

instance View ShowView where
    type ViewContextForView ShowView = ViewContext
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
                <li class="breadcrumb-item active">Show Post</li>
            </ol>
        </nav>
        <h1>Show Post</h1>
    |]
```

We can see that the `ShowView` is just a data definition. There is also an `View ShowView` instance. The html-like syntax inside the `html` function is, what we call it, `hsx` code. It's just like react's jsx. You can write html code as usual there. Everything inside the `[hsx|my html|]` block is also type-checked and converted to haskell code at compile-time.

Now that we have a rough overview of all the parts belonging to our `Post`, it's time to do some coding ourselves.

### 5. Extending the Blog

The generated controller already feels close to a super simple blog. Now it's time to make it more beautiful.

#### Creating a Post

First we quickly need to create a new blog post. Open [http://localhost:8000/Posts](http://localhost:8000/Posts) and click on "+ New". Then enter `Hello World!` into the "Title" field and `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam` into "Body".

Click "Save Post". You should now see the new post listed on the index view.

#### Displaying a Post

Let's first improve the show view. Right now the headline is "Show Post", and the actual Post body is never shown.

Open the `src/Apps/Web/View/Posts/Show.hs` and replace `<h1>Show Post</h1>` with `<h1>{get #title post}</h1>`. Also add a `<div>{get #body post}</div>` below the `<h1>`.

The hsx code should now look like this:
```html
<nav>
    <ol class="breadcrumb">
        <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
        <li class="breadcrumb-item active">Show Post</li>
    </ol>
</nav>
<h1>{get #title post}</h1>
<div>{get #body post}</div>
```

After you saved the changes, you should see that the changes have been reflected in the browser already. In the background the page has been refreshed automatically (technically the page has been patched using a vdom library).

#### Display all posts

After creating your post, you should have already seen that posts list is right now displaying all the post fields. Let's change it to only display the post's title.

Open the `src/Apps/Web/View/Posts/Index.hs` and replace `<td>{post}</td>` with `<td>{get #title post}</td>`.

Let's also make it clickable by wrapping it in a link. We can just put a `<a href={ShowPostAction (get #id post)}>` around it. The line should now look like `<td><a href={ShowPostAction (get #id post)}>{get #title post}</a></td>`.

Now we can also remove the "Show" link. We can do that by removing the next line `<td><a href={ShowPostAction (get #id post)}>Show</a></td>`.


#### Adding Validation

Let's make sure that every post has atleast a title. Validations can be defined inside `src/Apps/Web/Validation.hs`.

You should see an instance like this:

```haskell
instance ValidateRecord NewPost ControllerContext where
    validateRecord = do
        validateNothing
```

Replace `validateNothing` with `validateField #title nonEmpty`.

The instance should now look like this:

```haskell
instance ValidateRecord NewPost ControllerContext where
    validateRecord = do
        validateField #title nonEmpty
```

Now open [http://localhost:8000/Posts/new](http://localhost:8000/Posts/new) and click "Save Post" without filling the text fields. You will get a "This field cannot be empty".

That's how easy it is, to validate your models with interactive lambda.

#### Timestamps

It would be nice to always show the latest post on the index view. Let's add a timestamp to do exactly that.

Before we change our database schema, it's time to quickly save our current database state. For that you can just run `make dump_db`:

```bash
$ make dump_db
```

Take a look at `src/Model/Fixtures.sql`, the file should look like this now:

```sql
-- .......
-- .......

INSERT INTO public.posts VALUES ('fcbd2232-cdc2-4d0c-9312-1fd94448d90a', 'Hello World!', 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam');

```

All our existing posts are saved here. You can also commit this file to git to share your fixtures with your team mates. We will need the saved fixtures in a moment.

Let's add our timestamp column. Open `src/Model/Schema.hs` and add `+ field "created_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }` to the `posts` table. We set the column value to `NOW()` by default, so the `created_at` field is automatically set to the current time.

```haskell
table "posts"
    + field "id" primaryKey
    + field "title" text
    + field "body" text
    + field "created_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }
```

Now open the browser again. You will see `Something went wrong`. In the dev server console you will see something along `mismatch between number of columns to convert and number in target type`.

The application now expects the `created_at` field to be set inside the database. Our `posts` table does not know about that column yet.

Run the following command to fix that:
```bash
$ make db
```

This command will destroy the database, reload the schema and then insert the fixtures. The last step is the reason why we saved our database state to `src/Model/Fixtures.sql` a moment ago.

You can open [http://localhost:8000/Posts](http://localhost:8000/Posts) again. The error is gone now.

Now we can order the posts by our new `created_at` field. Open `src/Apps/Web/Controller/Posts.hs` and add `orderByDesc #createdAt` like this inside the `action PostsAction`:

```haskell
action PostsAction = do
    posts <- query @Post
        |> orderByDesc #createdAt
        |> fetch
    render IndexView { .. }
```

Let's also show the creation time in the `ShowView` in `src/Apps/Web/View/Posts/Show.hs`. There we add `<p>{timeAgo (get #createdAt post)}</p>` below the title:

```html
<nav>
    <ol class="breadcrumb">
        <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
        <li class="breadcrumb-item active">Show Post</li>
    </ol>
</nav>
<h1>{get #title post}</h1>
<p>{timeAgo (get #createdAt post)}</p>
<div>{get #body post}</div>
```

Open the view to check that it's working. If everything is fine, you will see something like `5 minutes ago` below the title. The `timeAgo` helper uses a bit of javascript to automatically displays the given timestamp in the current time zone and in a relative format. In case you want to show the absolute time (like `10.6.2019, 15:58 Uhr`), just use `dateTime` instead of `timeAgo`.

# QueryBuilder Reference

You can compose database queries using the QueryBuilder module.

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

### `findById id`
Just a shortcut for `filterWhere (#id, id) |> fetchOne`

```haskell
-- Long version
project <- query @Project |> filterWhere (#id, id) |> fetchOne
-- Shorter version
project <- query @Project |> findOneById #id id
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

## Raw SQL Queries
```haskell
result <- sqlQuery "SELECT * FROM projects WHERE id = ?" (Only id)
```

# Validation Reference

## Pure Validations

With `validateField` you can do simple, pure validations.


Here are some examples:

```haskell
instance ValidateRecord NewPost ControllerContext where
    validateRecord = do
        validateField #title nonEmpty
        validateField #phone isPhoneNumber
        validateField #email isEmail
        let isAge = isInRange (0, 100) in validateField #age isAge
````

### Custom Validators

If needed you can just write your own constraint, e.g. like this:

```haskell
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
nonEmpty _ = Success

isAge :: Int -> ValidatorResult
isAge = isInRange (0, 100)
```

## Uniqueness

The function `validateIsUnique` is usually used to make sure that e.g. a user's email is unique. Of course it will also work with other model fields.

```haskell
instance ValidateRecord NewPost ControllerContext where
    validateRecord = do
        validateIsUnique #email
````

## Validate Permissions

```haskell
instance ValidateRecord NewPost ControllerContext where
    validateRecord = do
        validateCanView currentUser
````

# View Helper Reference

## Time

### `timeAgo`

```haskell
timeAgo (get #createdAt post) -- "1 minute ago"
```

### `dateTime`

```haskell
dateTime (get #createdAt post) -- "10.6.2019, 15:58 Uhr"
```