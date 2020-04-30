# Getting Started With Turbo Haskell
Turbo Haskell is a full stack framework focused on rapid application development while striving for robust code quality.

This guide covers everything you need to ship software with interactive lambda. 

##### Feature Overview

- **Fully managed dev environment:** Works on any machine because all dependencies (including PostgreSQL) are managed using the nix package manager.
- **Bootstrap 4 Out of the box:** The default layout already integrates bootstrap 4.
- **Auto live reload using virtual dom in dev mode:** Each code change changes your local page to refresh. The refresh uses a diff based patch to avoid resetting the page state.
- **Build robust applications:** With the power of haskell your application are going to be a lot more robust. Pretty much no runtime errors in production.
- **Fast dev enviroment:** While we use a compiled language, the built-in dev server automatically reloads your code changes using the fastest way possible. Usually changes are reflected in less than 100ms (alot faster than your average webpack setup).
- **Very fast production environment:** Production response times around 30ms. Using [instant click](http://instantclick.io/) it can be faster than your average SPA.


```toc
```



## Dependencies

### 1. Nix Package Manager

The framework uses the nix package manager to manage the whole set of dependencies of your application

For example postgresql and the haskell compiler are both dependencies of your app. as well as all the haskell or javascript packages you want to use. we use nix to make sure that these dependencies are available to the app - in development, as well as in production.

That's why we first need to make sure that you have nix installed.

##### MacOS, Linux

Install nix by running the following command in your shell and follow the instructions on the screen:

```bash
curl https://nixos.org/nix/install | sh
```

There are also other ways to install nix, [take a look at the documentation](https://nixos.org/nix/download.html).


##### Windows
Sorry, we don't support windows yet.

### 2. Direnv

TurboHaskell uses `direnv` to speed up the development shell. As it needs to be hooked into your shell, it needs to be installed manually.

Install it via nix:

```bash
nix-env -i direnv
```

**After that you also need to hook it into your shell:**

##### Bash

If you use bash, add the following line at the end of the `~/.bashrc` file:

```bash
eval "$(direnv hook bash)"
```

##### ZSH

If you use zsh, add the following line at the end of the `~/.zshrc` file:

```bash
eval "$(direnv hook zsh)"
```

##### Other shell

For other shells, [take a look at the direnv documentation](https://direnv.net/#README).

### 2. Installing TurboHaskell

You can now install interactive lambda by running:

```bash
$ nix-env -f https://turbohaskell.digitallyinduced.com/turbohaskell-new.tar.gz -i turbohaskell-new
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
| Application/                          | Your domain logic lives here                                           |
| Application/Schema.hs           | Models and database tables are defined here                                 |
| Web/Controller       | Web application controllers                                                             |
| Web/View/            | Web application html template files                                                         |
| Web/Types.hs            | Central place for all web application types                                                         |
| static/                       | Images, css and javascript files                                            |
| .ghci                         | Default config file for the haskell interpreter                             |
| .gitignore                    | List of files to be ignored by git                                          |
| App.cabal, Setup.hs           | Config for the cabal package manager (TODO: maybe move to Config/App.cabal) |
| default.nix                   | Declares your app dependencies (like package.json or composer.json)         |
| Makefile                      | Default config file for the make build system                               |

### 2. Hello, World!

You now already have a working haskell app ready to be started.
You can start the development server by running this in the `Blog` directory:

```bash
$ make
```


Your app is now running at [http://localhost:8000](http://localhost:8000).
The server can be stopped by pressing CTRL+C.

The built-in development server automatically starts a PostgreSQL database connected to your application. So you don't need to worry about manually setting up the database.


### 3. Data Structures & PostgreSQL

##### Schema Modeling

For our blog we're going to deal with posts. A post has a title and a body and of course also an id. TurboHaskell is using UUIDs instead of the typical numerics ids.

**A `posts` table in a PostgreSQL database could loke like this:**


| id :: UUID                           | title :: Text                                          | body :: Text                                                                                                       |
|--------------------------------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| 8d040c2d-0199-4695-ac13-c301970cff1d | My Experience With Nix On OS X                         | Some time ago, I’ve moved this jekyll-based blog ...                                                               |
| b739bc7c-5ed1-43f4-944c-385aea80f182 | Deploying Private GitHub Repositories To NixOS Servers | In a previous post I’ve already shared a way to deploy private git repositories to a NixOS based server. While ... |


To work with posts in our application, we now have to declare this data schema.
Open `Application/Schema.hs` and add the following code:


```haskell
module Application.Schema where
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

Take a look at `Application/Schema.sql`. The dev server has auto-generated a `CREATE TABLE`-statement for us:

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Please don't make any modifications to this file as it's auto generated. Use Application/Schema.hs to change the schema
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

TurboHaskell uses controllers to deal with incoming requests. We can use the built-in code generators to generate an empty controller for our posts.

A controller belongs to an application. Your whole project can consistent of multiple sub applications. Typically your production app will need e.g. an admin backend application next to the default web application.

We can run the code generator like this:

```bash
$ new-controller posts

+ Web/Controller/Posts.hs
* Web/Routes.hs
* Web/Types.hs
* Web/App.hs (import)
* Web/App.hs (import)
+ Web/View/Posts/Show.hs
+ Web/View/Posts/New.hs
+ Web/View/Posts/Edit.hs
+ Web/View/Posts/Index.hs
``` 

You can see that lot's of files have been created and updated.

Open your browser at [http://localhost:8000/Posts](http://localhost:8000/Posts) to try out the new controller. The generator did all the initial work we need to get our usual CRUD actions going.

Here's how the new controller looks like:

![Screenshot of the /Posts view](images/Posts.png)

##### New Types

Let's first take a closer look at the changes in `Web/Types.hs`. Here a new data structures was created:


```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

We have one constructor for each possible action. Here you can see a short description for all the constructors:

| Action                                 | Request                    | Description                        |
|----------------------------------------|----------------------------|------------------------------------|
| `PostsAction`                          | `GET /Posts`               | Lists all posts                    |
| `NewPostAction`                        | `GET /NewPost`           | Displays the form to create a post |
| `ShowPostAction { postId = someId }`   | `GET /ShowPost?postId={someId}`      | Shows the posts with id $someId    |
| `CreatePostAction`                     | `POST /CreatePost`              | Endpoint to create a Post          |
| `EditPostAction { postId = someId }`   | `GET /EditPost?postId={someId}` | Displays the form to edit a post   |
| `UpdatePostAction { postId = someId }` | `POST /UpdatePost?postId={someId}`     | Endpoint to submit a post update   |
| `DeletePostAction { postId = someId }` | `DELETE /DeletePost?postId={someId}`   | Deletes the post                   |


A request like "Show me the post with id `e57cfb85-ad55-4d5c-b3b6-3affed9c662c`" can be represented like `ShowPostAction { postId = e57cfb85-ad55-4d5c-b3b6-3affed9c662c }`.

The type `Id Post` is just a UUID, but wrapped within a newtype (`newtype Id model = Id UUID`).


##### Controller Implementation: `Web/Controller/Posts.hs`

The actual code running, when an action is executed, is defined in `Web/Controller/Posts.hs`. Let's take a look, step by step.

###### Imports

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
```

In the header we just see some imports. Controllers always import a special `Web.Controller.Prelude` module. It provides e.g. controller helpers and also the framework specific functions we will see below. The controller also imports all its views. Views are also just "normal" haskell modules.

###### Instance


```haskell
instance Controller PostsController where
```

The controller logic is specified by implementing an instance of the `Controller` type-class.

###### Index Action

This is where the interesting part begins. As we will see below, the controller implementation is just an `action` function, pattern mattching over our `data PostsController` structure we defined in `Web/Types.hs`.

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

This is our endpoint for `/NewPost`. It just creates an empty new post and then passes it to the `NewView`. The `newRecord` is giving us an empty `Post` model. It's equivalent to manually writing `Post { id = Default, title = "", body = "" }`.

###### Show Action

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```
This is our show action at `/ShowPost?postId=postId`. Here we pattern match on the `postId` field of `ShowPostAction` to get post id of the given request. Then we just call `fetch` on that `postId` which gives us the specific `Post` record. Then we just pass that post to the view.

###### Edit Action

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }
```
Our `/EditPost?postId=postId` action. It's pretty much the same as in the `action ShowPostAction`, just with a different view.


###### Update Action

```haskell
    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }
```

This action deals with update requests for a specific post. As usual we pattern match on the `postId` and fetch it. 

The interesting part is `buildPost`. It is a helper function defined later in the controller: `buildPost = fill @["title", "body"]`. The `fill` call inside `buildPost` reads the `title` and `body` attributes from the browser request and fills them into the `post` record. The `buildPost` is also the place for validation logic.

`ifValid` returns `Either Post Post`. `Left post` means that e.g. the `title` or `body` did not pass validation. `Right post` means that all parameters could be set on `post` without any errors. 

In the error case (`Left post ->`) we just re-render the `EditView`. The `EditView` then tells the user about validation errors.

In the success case (`Right post ->`) we save the updated post to the database (with `updateRecord`). Then we set a success message and redirect the user back to the edit view.


```haskell
    action CreatePostAction = do
        let post = newRecord @NewPost
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction
```

Our create action, dealing with `POST /CreatePost` requests.

It's pretty much like the update action. When the validation succeeded, it saves the record to the database using `createRecord`.

```haskell
    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction
```

The last action is dealing with `DELETE /DeletePost?postId=postId` requests. It's pretty much like the other actions, we just call `deleteRecord` here.

##### Routes

The router is configured in `Web/Routes.hs`. The generator just places a single line there:

```haskell
instance AutoRoute PostsController
```

This empty instance magically sets up the routing for all the actions. Later you will learn how you can customize the urls according to your needs (e.g. "beautiful urls" for SEO).

##### Views

We should also quickly take a look at our views.

Let first look at the show view in `Web/View/Posts/Show.hs`:

```haskell
module Web.View.Posts.Show where
import Web.View.Prelude

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

Open the `Web/View/Posts/Show.hs` and replace `<h1>Show Post</h1>` with `<h1>{get #title post}</h1>`. Also add a `<div>{get #body post}</div>` below the `<h1>`.

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

Open the `Web/View/Posts/Index.hs` and replace `<td>{post}</td>` with `<td>{get #title post}</td>`.

Let's also make it clickable by wrapping it in a link. We can just put a `<a href={ShowPostAction (get #id post)}>` around it. The line should now look like `<td><a href={ShowPostAction (get #id post)}>{get #title post}</a></td>`.

Now we can also remove the "Show" link. We can do that by removing the next line `<td><a href={ShowPostAction (get #id post)}>Show</a></td>`.


#### Adding Validation

Let's make sure that every post has atleast a title. Validations can be defined inside our controller `Web/Controller/Posts.hs`.

Right now at the bottom of the file we have this:


```haskell
buildPost = fill @["title","body"]
```

Replace the implementation with this:

```haskell
buildPost post = post
    |> fill @["title","body"]
    |> validateField #title nonEmpty
```

Now open [http://localhost:8000/Posts/new](http://localhost:8000/Posts/new) and click "Save Post" without filling the text fields. You will get a "This field cannot be empty".

That's how easy it is, to validate your models with TurboHaskell.

#### Timestamps

It would be nice to always show the latest post on the index view. Let's add a timestamp to do exactly that.

Before we change our database schema, it's time to quickly save our current database state. For that you can just run `make dumpdb`:

```bash
$ make dumpdb
```

Take a look at `Application/Fixtures.sql`, the file should look like this now:

```sql
-- .......
-- .......

INSERT INTO public.posts VALUES ('fcbd2232-cdc2-4d0c-9312-1fd94448d90a', 'Hello World!', 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam');

```

All our existing posts are saved here. You can also commit this file to git to share your fixtures with your team mates. We will need the saved fixtures in a moment.

Let's add our timestamp column. Open `Application/Schema.hs` and add `+ field "created_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }` to the `posts` table. We set the column value to `NOW()` by default, so the `created_at` field is automatically set to the current time.

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

This command will destroy the database, reload the schema and then insert the fixtures. The last step is the reason why we saved our database state to `Application/Fixtures.sql` a moment ago.

You can open [http://localhost:8000/Posts](http://localhost:8000/Posts) again. The error is gone now.

Now we can order the posts by our new `created_at` field. Open `Web/Controller/Posts.hs` and add `orderByDesc #createdAt` like this inside the `action PostsAction`:

```haskell
action PostsAction = do
    posts <- query @Post
        |> orderByDesc #createdAt
        |> fetch
    render IndexView { .. }
```

Let's also show the creation time in the `ShowView` in `Web/View/Posts/Show.hs`. There we add `<p>{timeAgo (get #createdAt post)}</p>` below the title:

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

#### Markdown

Right now our posts can only be plain text. Let's make it more powerful by adding support for markdown.

##### Adding a Markdown Library

To deal with markdown, instead of implementing our own markdown parser, let's just use an existing package. There's the excellt `mmark` package we can use.

To install this package, open the `default.nix` file and append `mmark` to the `haskellDeps` list. The file will now look like this:

```nix
let
    haskellEnv = import ./TurboHaskell/NixSupport/default.nix {
        compiler = "ghc844";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            directory
            string-conversions
            wai
            mtl
            blaze-html
            blaze-markup
            wai
            mtl
            text
            postgresql-simple
            wai-util
            aeson
            uuid
            hlint
            parsec
            template-haskell
            interpolate
            uri-encode
            generic-lens
            tz
            turbohaskell
            mmark
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

Now restart the development server by pressing CTRL+C and then typing `make` again. We will see that mmark is going to be installed.

##### Markdown Rendering

Now that we have `mmark` installed, we need to integrate it into our `ShowView`. First we need to import it: Add `import qualified Text.MMark as MMark` to the top of `Web/View/Posts/Show.hs`.

Next change `{get #body post}` to `{get #body post |> renderMarkdown}`. This pipes the body field through a function `renderMarkdown`. Of course we also have to define the function now.

Add the following to the bottom of the show view:
```haskell
renderMarkdown text = text
```

This function now does nothing except return it's input text. Our markdown package provides two functions, `MMark.parse` and `MMark.render` to deal with the markdown. Let's first deal with parsing:

```haskell
renderMarkdown text = text |> MMark.parse ""
```
The empty string we pass to `MMark.parse` is usually the file name of the `.markdown` file, as we don't have any markdown file, we just pass an empty string.

Now open the web app and take a look at a blog post. You will see something like this:

```
Right MMark {..}
```

This is the parsed representation of the markdown. Of course that's not very helpful. We also have to connect it with `MMark.render` to get html code for our markdown. Replace the `renderMarkdown` with the following code:

```haskell
renderMarkdown text =
    case text |> MMark.parse "" of
        Left error -> "Something went wrong"
        Right markdown -> MMark.render markdown |> tshow |> preEscapedToHtml
```

The show view will now show real formatted text, as we would have expected.

##### Forms & Validation

Let's also quickly update our form. Right now we have a one-line text field there. We can replace it with a textarea to support multi line text.

Open `Web/View/Posts/Edit.hs` and change `{textField #body}` to `{textareaField #body}`. We can also add a short hint that the text area supports markdown: Replace `{textareaField #body}` with `{(textareaField #body) { helpText = "You can use markdown here"} }`.

```haskell
renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {textField #title}
    {(textareaField #body) { helpText = "You can use markdown here"} }
    {submitButton}
|]
```

After that, do the same in `Web/View/Posts/New.hs`.

We can also add an error message when the user tries to save invalid markdown. We can quickly write a custom validator for that:

Open `Web/Controller/Posts.hs` and import `MMark` at the top:

```haskell
import qualified Text.MMark as MMark
```

Then add this custom validator to the bottom of the file:

```haskell
isMarkdown :: Text -> ValidatorResult
isMarkdown text =
    case MMark.parse "" text of
        Left _ -> Failure "Please provide valid markdown"
        Right _ -> Success
```

We can use the validator by adding a new `validateField #body isMarkdown` line to the `buildPost` function:

```haskell
buildPost post = post
    |> fill @["title","body"]
    |> validateField #title nonEmpty
    |> validateField #body nonEmpty
    |> validateField #body isMarkdown
```

Create a new post with just `#` (a headline without any text) as the content to see our new error message.

### 6. A Second Model

#### 6.1 Schema Modeling

It's time to add comments to our blog. For that open the `Schema.hs` and add a new table `comments` with the fields `id`, `post_id`, `author` and `body`:

```haskell
table "comments"
    + field "id" primaryKey
    + field "post_id" uuid { references = Just "posts", onDelete = Cascade }
    + field "author" text
    + field "body" text
```

The `uuid { references = Just "posts", onDelete = Cascade }` column type specifies that we have a uuid column including a foreign key constraint to the `posts` table. The `onDelete` option is set to cascade to tell our database to delete the comments when the post is removed.

The `Application/Schema.hs` will now look like this:


```haskell
module Application.Schema where
import ClassyPrelude (Maybe (..), (<>), Bool (..))
import TurboHaskell.SchemaSupport

database = [
    table "posts"
        + field "id" primaryKey
        + field "title" text
        + field "body" text
        + field "created_at" timestamp { defaultValue = Just (SqlDefaultValue "NOW()") }
        ,
    table "comments"
        + field "id" primaryKey
        + field "post_id" uuid { references = Just "posts", onDelete = Cascade }
        + field "author" text
        + field "body" text
    ]
```

#### 6.2 Loading the Schema

Run `make dump_db` and `make db` to save our current posts to `Application/Fixtures.sql` and to rebuild the database to add our new `comments` table.


#### 6.3 The Controller

It's time to also add a new controller for our comments. For that call the controller generator like this:

```bash
$ new-controller comments
```

This will generate a new working controller for us. We now need to do some adjustments to better integrate the comments into the posts.

---

Right now the comments are available at `/Comments`. As they are always connected to a post, we want to have them as a subresource of a post, e.g. like `/Posts/{postId}/Comments`.

Open `Web/Routes.hs` and change the `instance AutoRoute CommentsController` like this:

```haskell
instance AutoRoute (PostsController :> CommentsController)
```

The `:>` is a combinator to describe nested resource. In this case `PostsController` is the parent controller and `CommentsController` is the child controller.

---

After this change, the dev server will show some errors like this:

```haskell
Web/View/Comments/Show.hs:8:33: error:
    • Could not deduce (TurboHaskell.RouterSupport.AutoRoute CommentsController)
```

E.g. in the `Show.hs` the error is triggered by this line:
```html
<li class="breadcrumb-item"><a href={CommentsAction}>Comments</a></li>
```

The `CommentsAction` (inside the `href`) is of type `CommentsController`. Because this controller is now a child controller of `PostsController`, we have to write `ShowPostAction { id = thePostId } :> CommentsAction`. This expression is now of type `PostsController :> CommentsController`.

Once fixed, the line needs to look like this:
```html
<li class="breadcrumb-item"><a href={ShowPostAction { id = postId } :> CommentsAction}>Comments</a></li>
```

Of course now we also have to pass the `postId` to that view.

### Form Customization

### Login & Logout

# Debuging

TODO: Show example using traceShowId

# Architecture

This section tries to answer common questions on where to place your code. These are recommendations found by digitally induced to be working well.

In general remember that all specific web app logic should stay in the `Web/` space. The `Application/` space is for sharing code across all your different application. E.g. code shared between your web application and your admin backend.

##### Where to place a function I want to use in all my views?

If the function is only used in a single application and is a building block for your layout, place it in `Web/View/Layout.hs`. The module is already imported in all your views (just don't forget to add the function to the export list).

If the function is used across multiple applications or more like a helper function, place it in `Application/Helper/View.hs`. This module is also already included in your view files.

##### Where to place a function I want to use in all my controllers?

Place it in `Application/Helper/Controller.hs`. This module is already imported into your controllers.

##### Where to place a custom type?

Place it in `Web/Types.hs`.

##### Next to my main web application, I'm building an admin backend application. Where to place it?

A TurboHaskell project can consist of multiple applications. Run `new-application admin` to generate a new admin application. The logic for the new application is located in the `Admin/` directory. On the web you can find it at `http://localhost:8000/admin/` (all actions are prefixed with `/admin/`).

##### How to structure my CSS?

CSS files, as all your other static assets, should be place in the `static` directory.

Create an `static/app.css`. In there use CSS imports to import your other stylesheets. An example `app.css` could look like this:

```css
@import "/layout.css";
@import "/widget.css";
@import "/form.css";
@import "/button.css";
@import "/users.css";
```

In your `Web.View.Layout` just import the `app.css`:

```html
<link rel="stylesheet" href="/app.css"/>
```

###### Page-specific CSS rules

Place page-specific CSS used by e.g. views of the `Web.Controller.Users` controller in the `users.css`. Use [currentViewId](https://turbohaskell.digitallyinduced.com/api-docs/TurboHaskell-ViewSupport.html#v:currentViewId) to scope your css rules to the view.

Given the view:

```haskell
module Web.View.Projects.Show where

render = [hsx|
    <div id={currentViewId}>
        <h1>Hello World!</h1>
    </div>
|]
```

This will render like

```html
<div id="projects-show">
    <h1>Hello World!</h1>
</div>`
```

So in your `projects.css` you can just do rules like 

```css
#projects-show h1 { color: blue; }
```

###### SASS & Webpack

We discourage the use of tools like SASS or webpack because they have to much overhead.

###### Library CSS

CSS files from external libraries or components should be placed in `static/vendor/`.

##### How to structure my Javascript Code?

JS files, as all your other static assets, should be place in the `static` directory.

In general we follow an approach where most of the business logic resides on the haskell server. Only for small interactions we try to use a small isolated bit of javascript.

Your global, non-page specific, javascript code can be placed in `app.js`.

E.g. the `app.js` could look like this:

```javascript
$(function () {
    initNavbarEffects();
});

function initNavbarEffects() {
    // ...
}
```

In your `Web.View.Layout` just import the `app.js`:

```html
<script src="/app.js"></script>
```

###### Page-specific JS

Place page-specific JS used by e.g. views of the `Web.Controller.Users` controller in the `users.js`.

In the views, just import the javascript with `<script src="/users.js"></script>`.

###### Webpack

We discourage the use of webpack or any other bundler because they have to much overhead. Of course this advice only applies if you follow the approach to use as few javascript as possible.

###### Library JS

JS files from external libraries or components should be placed in `static/vendor/`. For simplicity it might make sense to just download the javascript bundle of the library you want to use, and then just commit it into git instead of using NPM.

For more complex use-cases with lots of javascript, you should not follow this advice and just use NPM instead.

##### Where to place static images?

Place your images in the `static` folder. We recommend to use SVG images.

# Reference

## QueryBuilder Reference

You can compose database queries using the QueryBuilder module.

### Creating a new query
To query the database for some records, you first need to build a query.
You can just use the `query` function for that.

```haskell
let myQueryBuilder = query
```

You can optionally specify the model you want to query:

```haskell
let myProjectQueryBuilder = query @Project
```

### Running a query

You can run a query using `fetch`, `fetchOneOrNothing` or `fetchOne`:

#### many rows: `fetch`
To run a query which will return many rows use `fetch`:
```haskell
example :: IO [Project]
example = do
    projects <- query @Project |> fetch
    -- Query: `SELECT * FROM projects`
    return projects
```

#### maybe single row: `fetchOneOrNothing`
To run a query which will maybe return a single row use `fetchOneOrNothing`:
```haskell
example :: IO (Maybe Project)
example = do
    project <- query @Project |> fetchOneOrNothing
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
```

#### single row: `fetchOne`
To run a query which will return a single and **throws an error if no record is found** row use `fetchOne`:
```haskell
example :: IO Project
example = do
    project <- query @Project |> fetchOne
    -- Query: `SELECT * FROM projects LIMIT 1`
    return project
```

### Where Conditions

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

### Order By

You can just use `orderBy #field`:
```haskell
projects <- query @Project
        |> orderBy #createdAt
        |> fetch
-- Query: `SELECT * FROM projects ORDER BY created_at`
```

### Or

```haskell
projects <- query @Project
         |> queryOr
            (filterWhere (#userId, userId)) (filterWhere (#teamId, teamId))
        |> fetch
-- Query: `SELECT * FROM projects WHERE (user_id = ?) OR (team_id = ?)`
```

### Union / Merging two queries

Two query builders of the same type can be merged like this:

```haskell
-- SELECT * FROM projects WHERE team_id = ?
let teamProjects :: QueryBuilder Project = query @Project |> filterWhere (#teamId, teamId)

-- SELECT * FROM projects WHERE team_id IS NULL AND created_by = ?
let personalProjects :: QueryBuilder Project = query @Project |> filterWhere (#teamId, Nothing) |> filterWhere (#createdBy, currentUserId)

-- SELECT * FROM projects WHERE (team_id = ?) OR (team_id IS NULL AND created_by = ?)
let projects :: QueryBuilder Project = queryUnion teamProjects personalProjects
```

### Shortcuts
#### `findBy #field value`
Just a shortcut for `filterWhere (#field, value) |> fetchOne`

```haskell
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOne
-- Shorter version
project <- query @Project |> findBy #userId userId
```

#### `findMaybeBy #field value`
Just a shortcut for `filterWhere (#field, value) |> fetchOneOrNothing`

```haskell
-- Long version
project <- query @Project |> filterWhere (#userId, userId) |> fetchOneOrNothing
-- Shorter version
project <- query @Project |> findMaybeBy #userId userId
```

#### `findById id`
Just a shortcut for `filterWhere (#id, id) |> fetchOne`

```haskell
-- Long version
project <- query @Project |> filterWhere (#id, id) |> fetchOne
-- Shorter version
project <- query @Project |> findOneById #id id
```

#### `findManyBy #field value`
Just a shortcut for `filterWhere (#field, value) |> fetch`

```haskell
-- Long version
projects <- query @Project |> filterWhere (#userId, userId) |> fetch
-- Shorter version
projects <- query @Project |> findManyBy #userId userId
```

### `projectId |> fetch`
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

### Raw SQL Queries
```haskell
result <- sqlQuery "SELECT * FROM projects WHERE id = ?" (Only id)
```

## Validation Reference

[See the API Documentation](https://turbohaskell.digitallyinduced.com/api-docs/TurboHaskell-ValidationSupport-ValidateField.html).

## View Helper Reference

### Time

#### `timeAgo`

```haskell
timeAgo (get #createdAt post) -- "1 minute ago"
```

#### `dateTime`

```haskell
dateTime (get #createdAt post) -- "10.6.2019, 15:58 Uhr"
```

## Command Line Reference
| Command                                 | Description                                                             |
|-----------------------------------------|-------------------------------------------------------------------------|
| make                                    | Starts the dev server                                                   |
| make db                                 | Creates a new database with the current Schema and imports Fixtures.sql |
| make dumpdb                             | Saves the current database state into the Fixtures.sql                  |
| make psql                               | Connects to the running postgresql server                               |
| ghci                                    | Opens a new ghci session with the project and framework already loaded  |
| make clean                              | Resets all build and temporary files                                    |
| make print-ghc-extensions               | Prints all used ghc extensions. Useful for scripting                    |
| make print-ghc-options                  | Prints all used ghc options. Useful for scripting                       |
| make build/bin/RunUnoptimizedProdServer | Quickly does a production build (all compiler optimizations disabled)   |
| make build/bin/RunOptimizedProdServer   | Full production build with all ghc optimizations (takes a while)        |
| make static/prod.js                     | Builds the production js bundle                                         |
| make static/prod.css                    | Builds the production css bundle                                        |


## Form Reference

### Select Inputs

You can use the `selectField` helper for select inputs:

```haskell
formFor project [hsx|
    {selectField #userId users}
|]
```
In the example above the variable `users` contains all the possible option values for the select.

You also need to define a instance `CanSelect User`:
```haskell
instance CanSelect User where
    -- Here we specify that the <option>-value should contain a UserId
    type SelectValue User = UserId
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
```

Given the above example, the rendered form will look like this:
```html
-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }]
<form ...>
    <select name="user_id">
        <option value="1">Marc</option>
        <option value="2">Andreas</option>
    </select>
</form>
```



# Recipes

This section describes best-practise solutions to common tasks your facing when building web applications.

## Static Pages

For adding a static page like e.g. a start page, terms of service, privacy, pricing etc. you usually use a normal controller which just renders the view for that page. The only special thing is, that you might want to customize the routing to have SEO-friendly urls.

Let's say we have a controller like this defined in `Web.Types`:

```haskell
data StaticController
    = AboutAction
    | TermsAction
    deriving (Eq, Show, Data)
```

The controller implementation will look like this in `Web.Controller.Static`:

```haskell
module Web.Controller.Static where

import Web.Controller.Prelude
import Web.View.Static.Terms
import Web.View.Static.About

instance Controller StaticController where
    action TermsAction = render TermsView
    action AboutAction = render AboutView
```

We can now customize the routing in `Web.Routes` by first deleting the `instance AutoRoute StaticController` statement to delete the auto generated routing configuration and append:

```haskell
instance HasPath StaticController where
    pathTo TermsAction = "/terms"
    pathTo AboutAction = "/about"

instance CanRoute StaticController where
    parseRoute' = 
        (string "/terms" <* endOfInput >> pure TermsAction)
        <|> (string "/about" <* endOfInput >> pure AboutAction)
```

Now the terms can be reached at `/terms` instead of `/Terms`. The about is at `/about` now, instead of `/About`.

## Adding a native dependency

Sometimes your project uses some other software tool which is not bundled with TurboHaskell by default. Because we're using nix, we can easily manage that dependency for our project.

Let's say we want to add imagemagick to transform and resize images uploaded by the users of our application.

All dependencies of our project are listed in `default.nix` at the root of the project directory. The file looks like this:

```nix
let
    turboHaskell = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${turboHaskell}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            turbohaskell
            wreq
        ];
        otherDeps = p: with p; [
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

We now just have to add `imagemagick` to `otherDeps`:

```nix
let
    turboHaskell = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "c6d40612697bb7905802f23b7753702d33b9e2c1";
    };
    haskellEnv = import "${turboHaskell}/NixSupport/default.nix" {
        compiler = "ghc865";
        haskellDeps = p: with p; [
            cabal-install
            base
            classy-prelude
            wai
            text
            hlint
            turbohaskell
            wreq
        ];
        otherDeps = p: with p; [

            imagemagick # <-----------------------
            
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

If running, stop your development server. Now run `make` again. This will install imagemagick locally to your project.

When you are inside the project with your terminal, you can also call `imagemagick` to see that it's available.

You can look up the package name for the software you dependend on inside the nixpkgs repository. [Just open it on GitHub](https://github.com/NixOS/nixpkgs) and use the GitHub search to look up the package name.

## Uploading a user profile picture

You can easily upload a user profile pictures using `uploadImageWithOptions` inside your `UpdateUserAction`:

```haskell
action UpdateUserAction { userId } = do
    user <- fetch userId
    accessDeniedUnless (userId == currentUserId)

    let profilePictureOptions = ImageUploadOptions
            { convertTo = "jpg"
            , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
            }

    user
        |> fill @["firstname", "lastname", "pictureUrl"]
        |> uploadImageWithOptions profilePictureOptions #pictureUrl
        >>= ifValid \case
            Left user -> render EditView { .. }
            Right user -> do
                user <- user |> updateRecord
                setSuccessMessage "Deine Änderungen wurden gespeichert."
                redirectTo EditUserAction { .. }
```

This accepts any kind of image file compatible with imagemagick, resize it, reduce the image quality, stripe all meta information and save it as jpg. The file is stored inside the `static/uploads` folder in the project (directory will be created if it does not exist).

In your view, just use the image url like `<img src={get #pictureUrl currentUser}/>`.

## Creating a custom validator

If needed you can just write your own constraint, e.g. like this:

```haskell
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
nonEmpty _ = Success

isAge :: Int -> ValidatorResult
isAge = isInRange (0, 100)
```

## Checking that an email is unique

Use [`validateIsUnique`](https://turbohaskell.digitallyinduced.com/api-docs/TurboHaskell-ValidationSupport-ValidateIsUnique.html#v:validateIsUnique).
