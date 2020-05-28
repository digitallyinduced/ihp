# Creating Your First Project

```toc
```

## 1. Project Setup

This guide will lead you to create a small blog application. To set up the project, open a terminal and type:

```bash
$ ihp-new blog
```

The first time you set up IHP, this command might take 10 - 15 minutes to install. Any further projects after that will be a lot faster because all the packages are already cached on your computer. While the build is running, take a look at ["What Is Nix"](https://engineering.shopify.com/blogs/engineering/what-is-nix) by Shopify to get a general understanding on how nix works.

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

Switch to the `blog` directory before doing the next steps:

```bash
$ cd blog
```

Start the development server by running this in the `blog` directory:

```bash
$ ./start
```

Your application is starting now. The development server will automatically launch the built-in IDE.
The server can be stopped by pressing CTRL+C.

In the background, the built-in development server starts a PostgreSQL database connected to your application. Don't worry about about manually setting up the database. It also runs a websocket server to power live reloads on file saves inside your app.


## 3. Data Structures & PostgreSQL

### Schema Modeling

For our blog project, let's first build a way to manage posts.

For working with posts, we first need to create a `posts` table inside our database. A single post has a title and a body and of course also an id. IHP is using UUIDs instead of the typical numerics ids.

**This is how our `posts` table can look like for our blog:**


| id :: UUID                           | title :: Text                                          | body :: Text                                                                                                       |
|--------------------------------------|--------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|
| 8d040c2d-0199-4695-ac13-c301970cff1d | My Experience With Nix On OS X                         | Some time ago, I’ve moved this jekyll-based blog ...                                                               |
| b739bc7c-5ed1-43f4-944c-385aea80f182 | Deploying Private GitHub Repositories To NixOS Servers | In a previous post I’ve already shared a way to deploy private git repositories to a NixOS based server. While ... |


To work with posts in our application, we now have to define this data schema.

**For the curious:** IHP has a built-in GUI-based schema designer. The schema designer will be used in the following sections of this tutorial. The schema designer helps to quickly built the [DDL](https://en.wikipedia.org/wiki/Data_definition_language) statements for your database schema without remembering all the Postgresql syntax and data types. But keep in mind: The schema designer is just a GUI tool to edit the `Application/Schema.sql` file. This file consist of DDL statements to build your database schema. The schema designer parses the `Application/Schema.sql`, applies changes to the syntax tree and then writes it back into the `Application/Schema.sql`. If you love your VIM, can you always skip the GUI and go straight to the code at `Application/Schema.sql`. If you need to do something advanced which is not supported by the GUI, just manually do it with your code editor of choice. IHP is built by terminal hackers, so don't worry, all operations can always be done from the terminal :-)

Open the [IHP Schema Designer](http://localhost:8001/ihp/Tables) and add a new table with `title` and `body` as text column. To do this click on the `New` button in the table view.

![Schema Designer New Table](images/first-project/new_table_view.png)

Enter the table name `posts` and click on `Create Table`.

In the right pane, you can see the columns of the newly created table. The id column has been automatically created for us.

Right click into the Columns pane and select `Add Column`:

![Schema Designer Add Column menu](images/first-project/add_column_menu.png)

Use this dialog to create the title and body columns.

After that, your schema should look like this:

![Schema Designer First Table](images/first-project/first_table.png)

        
### Loading the Schema

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

To load the table into our local postgres server, we need to click `Push to DB` in the Schema Designer (use `make db` from the command line).

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

### Record Types

By specificing the above schema, the framework automatically provides several types for us. Here is a short overview:

|        Type | `Post`                                                                     |
|------------:|----------------------------------------------------------------------------|
|  Definition | `data Post = Post { id :: Id Post, title :: Text, body :: Text }` |
| Description | A single record from the `posts` table                                                                      |
| Example     | `Post { id = cfefdc6c-c097-414c-91c0-cbc9a79fbe4f, title = "Hello World", body = "Some body text" } :: Post` |


|        Type | `Id Post`                                                               |
|------------:|-------------------------------------------------------------------------|
|  Definition | `newtype Id Post = Id UUID`                                             |
| Description | Type for the `id` field                                                 |
| Example     | `"5a8d1be2-33e3-4d3f-9812-b16576fe6a38" :: Id Post` |


**For the curious:** To dig deeper into the generated code, open the Schema Designer, right-click a table and click "Show Haskell Code".

## 4. Apps, Controllers, Views

IHP follows the well-known MVC structure. Controllers and actions are used to deal with incoming requests.

A controller belongs to an application. The default application is called `Web` (that's why all controller and views are located there). Your whole project can consistent of multiple sub applications. Typically your production app will need e.g. an admin backend application next to the default web application.

We can use the built-in code generators to generate an controller for our posts. Inside the dev server, click on `CODEGEN` to open the [Code Generator](http://localhost:8001/ihp/Generators). There you can see everything that can be generated. Click on Controller:

![](images/first-project/code_gen_1.png)

You need to enter the controller name. Enter `Posts` and click preview:

![](images/first-project/code_gen_2_posts.png)

The preview will show you all the files which are going to be created or modified. Take a look and when you are ready, click on `Generate`.

![](images/first-project/code_gen_3_posts.png)

After the files have been created as in the preview, your controller is already ready to be used. Open your browser at [http://localhost:8000/Posts](http://localhost:8000/Posts) to try out the new controller. The generator did all the initial work we need to get our usual CRUD actions going.

Here's how the new `/Posts` page looks like:

![Screenshot of the /Posts view](images/Posts.png)

Next we're going to dig a bit deeper into all the changes made by the controller generator.

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


A request like "Show me the post with id `e57cfb85-ad55-4d5c-b3b6-3affed9c662c`" can be represented like `ShowPostAction { postId = e57cfb85-ad55-4d5c-b3b6-3affed9c662c }`. Basically, the IHP router always maps a HTTP request to such an action data type. (By the way: The type `Id Post` is just a UUID, but wrapped within a newtype, `newtype Id model = Id UUID`).


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

#### Controller Instance


```haskell
instance Controller PostsController where
```

The controller logic is specified by implementing an instance of the `Controller` type-class.

#### Index Action

This is where the interesting part begins. As we will see below, the controller implementation is just an `action` function, pattern mattching over our `data PostsController` structure we defined in `Web/Types.hs`.

```haskell
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

This is the index action. It's called when opening `/Posts`. First it fetches all the posts from the database and then passes it along to the view. The `IndexView { .. }` is just shorthand for `IndexView { posts = posts }`.

#### New Action

```haskell
    action NewPostAction = do
        let post = newRecord
        render NewView { .. }
```

This is our endpoint for `/NewPost`. It just creates an empty new post and then passes it to the `NewView`. The `newRecord` is giving us an empty `Post` model. It's equivalent to manually writing `Post { id = Default, title = "", body = "" }`.

#### Show Action

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```
This is our show action at `/ShowPost?postId=postId`. Here we pattern match on the `postId` field of `ShowPostAction` to get post id of the given request. Then we just call `fetch` on that `postId` which gives us the specific `Post` record. Then we just pass that post to the view.

#### Edit Action

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }
```
Our `/EditPost?postId=postId` action. It's pretty much the same as in the `action ShowPostAction`, just with a different view.


#### Update Action

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

#### Create Action

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

#### Delete Action

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

instance View ShowView ViewContext where
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

We can see that the `ShowView` is just a data definition. There is also an `View ShowView` instance. The html-like syntax inside the `html` function is `hsx` code. It's just like react's jsx. You can write html code as usual there. Everything inside the `[hsx|my html|]` block is also type-checked and converted to haskell code at compile-time.

Now that we have a rough overview of all the parts belonging to our `Post`, it's time to do some coding ourselves.

## 5. Extending the Blog

The generated controller already feels close to a super simple blog. Now it's time to make it more beautiful.

### Creating a Post

First we quickly need to create a new blog post. Open [http://localhost:8000/Posts](http://localhost:8000/Posts) and click on "+ New". Then enter `Hello World!` into the "Title" field and `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam` into "Body".

Click "Save Post". You should now see the new post listed on the index view.

TODO: Add screenshot of the index view the content

### Displaying a Post

Let's first improve the show view. Right now the headline is "Show Post", and the actual Post body is never shown.

Open the `Web/View/Posts/Show.hs` and replace `<h1>Show Post</h1>` with `<h1>{get #title post}</h1>`. Also add a `<div>{get #body post}</div>` below the `<h1>`.

The `Web/View/Posts/Show.hs` file should look like this:
```html
module Web.View.Posts.Show where
import Web.View.Prelude

data ShowView = ShowView { post :: Post }

instance View ShowView ViewContext where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
                <li class="breadcrumb-item active">Show Post</li>
            </ol>
        </nav>
        <h1>{get #title post}</h1>
        <div>{get #body post}</div>
    |]

```

After you saved the changes, you should see that the changes have been reflected in the browser already. In the background the page has been refreshed automatically. This refresh is using a diff based approach by using [morphdom](https://github.com/patrick-steele-idem/morphdom).

### Display all posts

After creating your post, you should have already seen that posts list is right now displaying all the post fields. Let's change it to only display the post's title.

Open the `Web/View/Posts/Index.hs` and replace `<td>{post}</td>` with `<td>{get #title post}</td>`.

Let's also make it clickable by wrapping it in a link. We can just put a `<a href={ShowPostAction (get #id post)}>` around it. The line should now look like:
```html
<td><a href={ShowPostAction (get #id post)}>{get #title post}</a></td>
```

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

![Schema Designer Title non empty](images/first-project/title_non_empty.png)

You can find [a list of all available validator functions in the API Documentation](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html).

### Timestamps

It would be nice to always show the latest post on the index view. Let's add a timestamp to do exactly that.

Before we change our database schema, it's time to quickly save our current database state. [Open the schema designer](http://localhost:8001/ihp/Tables) and click the `DB to Fixtures` button (from the terminal: `make dumpdb`). After that, take a look at `Application/Fixtures.sql`. The file should look like this:

```sql
-- .......
-- .......

INSERT INTO public.posts VALUES ('fcbd2232-cdc2-4d0c-9312-1fd94448d90a', 'Hello World!', 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam');

```

All our existing posts are saved here. You should also commit this file to git to share your fixtures with your team mates. We will need these saved fixtures in a moment, when we want to update the database schema.

Let's add a new `created_at` column. Open [http://localhost:8001/ihp/Tables](http://localhost:8001/ihp/Tables), enter `created_at` and select Timestamp for the type. Also set the default value to `NOW()`.

![Schema Designer Timestamp column](images/first-project/timestamp_column.png)

Now open the `/Posts` again inside your browser. You will see this error:

![Database looks outdated. The database result does not match the expected type.](images/first-project/database_error.png)

This happens because we only added the `created_at` column to the `Application/Schema.sql` file by using the Schema Designer. But the actual running Postgres server still uses the older database schema.

To update the local postgres server, open the Schema Designer and click the `Push to DB` button. This button will destroy the database, reload the schema and then insert the fixtures. The last step is the reason why we saved our database state to `Application/Fixtures.sql` a moment ago.

In general the workflow for making database schema changes locally is: Make changes to the `Schema.sql`, Save database state with `DB to Fixtures`, Update Database with `Push to DB`.

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

![Schema Designer created at view](images/first-project/created_at_view.png)

### Markdown

Right now our posts can only be plain text. Let's make it more powerful by adding support for markdown.

#### Adding a Markdown Library

To deal with markdown, instead of implementing our own markdown parser, let's just use an existing package. There's the excellt `mmark` package we can use.

To install this package, open the `default.nix` file and append `mmark` to the `haskellDeps` list. The file will now look like this:

```nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        rev = "0d2924bcd4cde09e9f219f5e7eca888ad473094a";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            mmark # <--------- OUR NEW PACKAGE ADDED HERE
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
```

Update the local development environment by running `make -B .envrc`. This will download and install the mmark package. Now restart the development server by pressing CTRL+C and then typing `./start` again.

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

```html
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

It's time to add comments to our blog. For that open the Schema Designer and add a new table `comments` with the fields `id`, `post_id`, `author` and `body`:

![Schema Designer Comments](images/first-project/post_table.png)

When adding the post\_id column, it will automatically set the type to UUID. Unless you unselect the Checkbox `References posts` it will also automatically create a foreign key constraint for this column:

By default the foreign key constraint has set it's `ON DELETE` behavior to `NO ACTION`. To change the `ON DELETE`, click on the `FOREIGN KEY: posts` next to the `post_id` field.

### 6.2 Loading the Schema

Press `DB to Fixtures` to save our current posts to `Application/Fixtures.sql` and press the `Push to DB`-button to rebuild the database to add our new `comments` table.


### 6.3 The Controller

It's time to add a controller for our comments. We can use the visual code generator for this:

![](images/first-project/code_gen_1.png)

Use `Comments` as the controller name:

![](images/first-project/code_gen_2.png)

Click generate:

![](images/first-project/code_gen_3.png)


The controller is generated now. But we need to do some adjustments to better integrate the comments into the posts.