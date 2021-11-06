# Building Realtime Single Page Apps with IHP

```toc

```

## Introduction

While IHP has a strong preference for server-side rendering, specific functionality in your app might require interactivity that can only be implemented with Javascript. In these cases we recommend that you follow a hybrid approach: Implement interactive functionality as a small single page app, while keeping unrelated functionality like the login and signup still server side rendered.

The hybrid approach gives you the best of both worlds: The low interactivity of a single page app where needed, while keeping the simplicity and high performance advantages of server side rendered IHP apps.

This guide will help you understand the best-practices of building hybrid applications with IHP and React.

## Adding React to your IHP project

### Adding NodeJS to a project

To access the JavaScript ecosystem we need to add NodeJS to our project. You should also follow this step if you have NodeJS already installed on your system. Installing the NodeJS version via nix allows all people working on your project to get the exact NodeJS version you're using.

For that open your projects `default.nix` and add `nodejs` to `otherDeps`:

```nix
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
            nodejs
        ];
```

Now you need to rebuild your local dev environment:

```bash
nix-shell --run 'make -B .envrc'
```

After that, you have `node` and `npm` available in your project.

While nix can theoretically also install NPM modules, it's easier to just use NPM directly. Otherwise, you will spend lots of time with nix-related issues. Because `nodejs` and `npm` are managed by nix we still have reproducible builds. Every developer on your team will use the same NodeJS and NPM version.

### The `Frontend` directory

Create a new `Frontend` directory. We'll place all JS source files in that directory in the next steps:

```bash
mkdir Frontend
````

### NPM Init

Before we can install dependencies, we need to generate a `package.json`:

```bash
cd Frontend # npm needs to be run from the Frontend directory
npm init
```

Add the `Frontend/package.json` and the `Frontend/package.lock` to your git repository.

### Installing React

Now we can just install node modules as required by calling `npm install`:

```bash
cd Frontend # npm needs to be run from the Frontend directory

# Install react and react-dom
npm add react react-dom

# We also need esbuild for bundling
npm add esbuild
```

### Adding an Entrypoint

Next we need a new entrypoint for our SPA. Create a new file `Frontend/app.jsx` with the following content:

```javascript
import * as React from 'react'
import * as ReactDOM from 'react-dom'

class HelloWorld extends React.Component {
    render() {
        return <div>
            Hello from react!
        </div>
    }
}

function startApp() {
    ReactDOM.render(<HelloWorld/>, document.getElementById('hello-world'));
}

$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    startApp();
});

```

### Adding Make Tasks

Open your project's `Makefile` and append these tasks:

```Makefile
Frontend/node_modules:
    cd Frontend && npm install

static/app.js: Frontend/node_modules Frontend/app.jsx
    cd Frontend && ./node_modules/.bin/esbuild app.jsx --bundle --outfile=../static/app.js ${ESBUILD_FLAGS}

watch-frontend:
    touch Frontend/app.jsx # Force rebuild
    $(MAKE) static/app.js ESBUILD_FLAGS="--watch"
```

This will allow us to run `make static/app.js` to bundle `Frontend/app.jsx` with esbuild and save it to `static/app.js`.
The `make watch-frontend` command is a shorthand to run the bundler with the `--watch` flag.

### Running the Bundler

Open a new terminal tab and start the dev bundler:

```bash
make watch-frontend
```

This will recompile our `Frontend/app.jsx` whenever the file changes.

### Mounting the React Component

Open `Web/Static/Welcome.hs` (or any other IHP view) and add this div somewhere to the view:

```haskell
<div id="hello-world"/>
```

Now open the view in the browser. You should see `Hello from react!` on the page.

## Building a Realtime SPA with IHP DataSync

*The following section asumes that your app already has a login as described in the [Authentication](http://localhost:3000/authentication.html#setup) section.*

IHP DataSync is an IHP API that allows your to query your app's database from within JS. Basically it provides functions like `query`, `fetchOne` or `createRecord`, which you're already familiar with from the Haskell side, but makes them available in Javascript land.

E.g. you could fetch all posts of a user with this DataSync call:

```javascript
const posts = await query('posts')
    .orderBy('createdAt')
    .fetch()
```

This is very similiar to how you would do it on the Haskell backend side:

```haskell
posts <- query @Post
    |> orderBy #createdAt
    |> fetch
````

### Setting up the Schema

In this example we're going to build a simple todo list.

For that create a new table `todos` with the IHP Schema Designer. It needs the following structure:

```sql
CREATE TABLE todos (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    is_completed BOOLEAN DEFAULT false NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    user_id UUID NOT NULL
);
CREATE INDEX todos_user_id_index ON todos (user_id);
ALTER TABLE todos ADD CONSTRAINT todos_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
```

**Run `Update DB` to insert the new table into your local database.**

## Adding the `ihp_authenticated` role

IHP DataSync uses Postgres Row Level Security for making sure that users can only access rows that belong to them. The default database role used by IHP is also the owner of all the database tables. In postgres the table owner can always access all rows. Therefore IHP DataSync uses a second database role called `ihp_authenticated` to access the tables.

Follow these steps to set up the second database role.

1. Open `Web/FrontController.hs`
2. Add an import:
    ```haskell
    import qualified IHP.DataSync.Role as Role
    ```
3. Call `Role.ensureAuthenticatedRoleExists` from `initContext`:
    ```haskell
    instance InitControllerContext WebApplication where
        initContext = do
            setLayout defaultLayout
            initAutoRefresh
            initAuthentication @User

            -- ADD THIS CALL:
            Role.ensureAuthenticatedRoleExists
    ```
    The `Role.ensureAuthenticatedRoleExists` call makes sure that this second role exists and has permissions to access tables with Row level security policies applied.

## Enabling the DataSync Controllers

We also need to enable the DataSync controllers before we can use the APIs.

1. Open `Web/FrontController.hs`
2. Add these imports:
    ```haskell
    -- DataSync
    import IHP.DataSync.Types
    import IHP.DataSync.Controller
    import IHP.DataSync.REST.Types
    import IHP.DataSync.REST.Controller
    ```
3. Mount the controller:
    ```haskell
    instance FrontController WebApplication where
        controllers = 
            [ startPage WelcomeAction
            
            -- DataSync
            , webSocketApp @DataSyncController -- ADD THIS
            , parseRoute @ApiController        -- AND ALSO THIS
            
            -- Generator Marker
            ]
    ```
4. Open `Web/Routes.hs`
5. Add this import:
    ```haskell
    import IHP.DataSync.REST.Routes
    ```

## Loading the JS SDK

DataSync comes with a couple of JS files we need to load on our page before we can use it.

Open `Web/View/Layout.hs` and add these javascript files:

```haskell
scripts :: Html
scripts = [hsx|
        <!-- ... -->

        <!-- DataSync -->
        <script src={assetPath "/vendor/ihp-datasync.js"}></script>
        <script src={assetPath "/vendor/ihp-querybuilder.js"}></script>

        <-- Make sure that the datasync js files are included before the app.js -->

        <script src={assetPath "/app.js"}></script>
    |]
```

Now DataSync is enabled in your app.

Open your IHP app and then run this in the chrome JS console:

```javascript
await query('todos').fetch();
```

This will output an error like this:

```javascript
Uncaught {tag: 'DataSyncError', requestId: 1}
```


Take a look at the output of the IHP server. You will see a line with this:

```
Row level security is required for accessing this table
```

We can only access tables that have security policies defined. We didn't do that yet, and so IHP is blocking access to our `todos` table.

## Defining a Security Policy

1. Open `Application/Schema.sql`
2. Enable row level security for the `todos` table:
    ```sql
    ALTER TABLE todos ENABLE ROW LEVEL SECURITY;
    ```
3. Add a policy:
    ```sql
    CREATE POLICY "Users can manage their tasks" ON todos USING (user_id = ihp_user_id()) WITH CHECK (user_id = ihp_user_id());
    ```

    The `USING (user_id = ihp_user_id())` tells postgres that a row is only visibile if `user_id = ihp_user_id()`. The `ihp_user_id()` is a database function provided by IHP that returns the user id of the logged in user.

    The `CHECK (user_id = ihp_user_id())` is a validation that is run for INSERT and UPDATE queries. It makes sure that a user can only INSERT todos for himself, and cannot change the user id with an UPDATE query to move the todo to someone else.

Run `Update DB` to apply the schema changes to your local dev db.

## Fetching Data

Now open your local app again and try again to run the following JS snippet in the JS console:

```javascript
await query('todos').fetch();
```

This time it will output an empty array. That makes sense as we don't have any todos yet.

## Implementing our Todo List

TODO

## DataSync Operations

TODO