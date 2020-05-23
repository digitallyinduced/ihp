# Creating Your First Project

```toc
```

## 1. Project Setup

This guide will lead you to create a small blog application. To set up the project, open a terminal and type:

```bash
$ ihp-new blog
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
| Application/Schema.sql           | Models and database tables are defined here                                 |
| Web/Controller       | Web application controllers                                                             |
| Web/View/            | Web application html template files                                                         |
| Web/Types.hs            | Central place for all web application types                                                         |
| static/                       | Images, css and javascript files                                            |
| .ghci                         | Default config file for the haskell interpreter                             |
| .gitignore                    | List of files to be ignored by git                                          |
| App.cabal, Setup.hs           | Config for the cabal package manager (TODO: maybe move to Config/App.cabal) |
| default.nix                   | Declares your app dependencies (like package.json or composer.json)         |
| Makefile                      | Default config file for the make build system                               |

## 2. Hello, World!

You now already have a working haskell app ready to be started.
You can start the development server by running this in the `Blog` directory:

```bash
$ ./start
```


Your app is now running at [http://localhost:8000](http://localhost:8000).
The server can be stopped by pressing CTRL+C.

The built-in development server automatically starts a PostgreSQL database connected to your application. So you don't need to worry about manually setting up the database.


## 3. Data Structures & PostgreSQL

### Schema Modeling

For our blog we're going to deal with posts. A post has a title and a body and of course also an id. IHP is using UUIDs instead of the typical numerics ids.

**A `posts` table in a PostgreSQL database could loke like this:**


| id :: UUID                           | title :: Text                                          | body :: Text                                                                                                       |
|--------------------------------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| 8d040c2d-0199-4695-ac13-c301970cff1d | My Experience With Nix On OS X                         | Some time ago, I’ve moved this jekyll-based blog ...                                                               |
| b739bc7c-5ed1-43f4-944c-385aea80f182 | Deploying Private GitHub Repositories To NixOS Servers | In a previous post I’ve already shared a way to deploy private git repositories to a NixOS based server. While ... |


To work with posts in our application, we now have to declare this data schema.
Open [http://localhost:8001/ihp/Tables](http://localhost:8001/ihp/Tables) and add a new table with `title` and `body` as text column. To do this either click the button `New` in the table view
or right click inside of it and use `Add Column`.
The `id` column is generated automatically.

![Schema Designer First Table](images/first-project/first_table.png)

        
#### Loading the Schema

Next we need to make sure that our database schema with our `posts` table is imported into the local postgresql database. Don't worry, the local development postgresql server is already running. The dev server has conveniently already started it.

Open the `Application/Schema.sql` in your code editor to see the SQL queries which make up the database schema:

```sql
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL
);
```

To load the table into our local postgres server, we need to click `Push to DB`.

The `posts` table has been created now. Let's quickly connect to our database and see that everything is correct:

```bash
$ make psql

psql (9.6.12)
Type "help" for help.

-- Let's do a query to check that the table is there

app=# SELECT * FROM posts;

 id | title | body
----+-------+------
(0 rows)

-- Look's alright! :)



-- We can quit the postgresql console by typing \q

app=# \q
```

Now our database is ready to be consumed by our app.

#### Record Types

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

## 4. Apps, Controllers, Views

IHP uses controllers to deal with incoming requests. We can use the built-in code generators to generate an empty controller for our posts.

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

### New Types

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


### Controller Implementation: `Web/Controller/Posts.hs`

The actual code running, when an action is executed, is defined in `Web/Controller/Posts.hs`. Let's take a look, step by step.

#### Imports

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
```

In the header we just see some imports. Controllers always import a special `Web.Controller.Prelude` module. It provides e.g. controller helpers and also the framework specific functions we will see below. The controller also imports all its views. Views are also just "normal" haskell modules.

#### Instance


```haskell
instance Controller PostsController where
```

The controller logic is specified by implementing an instance of the `Controller` type-class.

##### Index Action

This is where the interesting part begins. As we will see below, the controller implementation is just an `action` function, pattern mattching over our `data PostsController` structure we defined in `Web/Types.hs`.

```haskell
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

This is the index action. It's called when opening `/Posts`. First it fetches all the posts from the database and then passes it along to the view. The `IndexView { .. }` is just shorthand for `IndexView { posts = posts }`.

##### New Action

```haskell
    action NewPostAction = do
        let post = newRecord
        render NewView { .. }
```

This is our endpoint for `/NewPost`. It just creates an empty new post and then passes it to the `NewView`. The `newRecord` is giving us an empty `Post` model. It's equivalent to manually writing `Post { id = Default, title = "", body = "" }`.

##### Show Action

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```
This is our show action at `/ShowPost?postId=postId`. Here we pattern match on the `postId` field of `ShowPostAction` to get post id of the given request. Then we just call `fetch` on that `postId` which gives us the specific `Post` record. Then we just pass that post to the view.

##### Edit Action

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }
```
Our `/EditPost?postId=postId` action. It's pretty much the same as in the `action ShowPostAction`, just with a different view.


##### Update Action

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

#### Routes

The router is configured in `Web/Routes.hs`. The generator just places a single line there:

```haskell
instance AutoRoute PostsController
```

This empty instance magically sets up the routing for all the actions. Later you will learn how you can customize the urls according to your needs (e.g. "beautiful urls" for SEO).

#### Views

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

## 5. Extending the Blog

The generated controller already feels close to a super simple blog. Now it's time to make it more beautiful.

### Creating a Post

First we quickly need to create a new blog post. Open [http://localhost:8000/Posts](http://localhost:8000/Posts) and click on "+ New". Then enter `Hello World!` into the "Title" field and `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam` into "Body".

Click "Save Post". You should now see the new post listed on the index view.

### Displaying a Post

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

### Display all posts

After creating your post, you should have already seen that posts list is right now displaying all the post fields. Let's change it to only display the post's title.

Open the `Web/View/Posts/Index.hs` and replace `<td>{post}</td>` with `<td>{get #title post}</td>`.

Let's also make it clickable by wrapping it in a link. We can just put a `<a href={ShowPostAction (get #id post)}>` around it. The line should now look like `<td><a href={ShowPostAction (get #id post)}>{get #title post}</a></td>`.

Now we can also remove the "Show" link. We can do that by removing the next line `<td><a href={ShowPostAction (get #id post)}>Show</a></td>`.


### Adding Validation

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

That's how easy it is, to validate your models with IHP.

### Timestamps

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

Let's add our timestamp column. Open [http://localhost:8001/ihp/Tables](http://localhost:8001/ihp/Tables) and add a new Timestamp
column

![Schema Designer Timestamp column](images/first-project/timestamp_column.png)

Now open the browser again. You will see `Something went wrong`. In the dev server console you will see something along `mismatch between number of columns to convert and number in target type`.

You need to press `Push to DB` to update the database.

This button will destroy the database, reload the schema and then insert the fixtures. The last step is the reason why we saved our database state to `Application/Fixtures.sql` a moment ago.

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

### Markdown

Right now our posts can only be plain text. Let's make it more powerful by adding support for markdown.

#### Adding a Markdown Library

To deal with markdown, instead of implementing our own markdown parser, let's just use an existing package. There's the excellt `mmark` package we can use.

To install this package, open the `default.nix` file and append `mmark` to the `haskellDeps` list. The file will now look like this:

```nix
let
    haskellEnv = import ./IHP/NixSupport/default.nix {
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
            template-haskell
            interpolate
            uri-encode
            generic-lens
            tz
            ihp
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

#### Markdown Rendering

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

#### Forms & Validation

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

## 6. A Second Model

### 6.1 Schema Modeling

It's time to add comments to our blog. For that open the `Schema.hs` and add a new table `comments` with the fields `id`, `post_id`, `author` and `body`:

![Schema Designer Comments](images/first-project/post_table.png)

To create the foreign key Constraint add the post\_id column as UUID:

![Schema Designer post id](images/first-project/new_column_post_id.png)

and then right click on the column post\_id to create the constraint through `Add foreign key constraint`:

![Schema Designer foreign key constraint](images/first-project/foreign_key.png)

The `onDelete` option is set to cascade to tell our database to delete the comments when the post is removed.

### 6.2 Loading the Schema

Run `make dump_db` and press the `Push to DB`-button to save our current posts to `Application/Fixtures.sql` and to rebuild the database to add our new `comments` table.


### 6.3 The Controller

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
    • Could not deduce (IHP.RouterSupport.AutoRoute CommentsController)
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
