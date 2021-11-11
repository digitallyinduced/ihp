# Building Realtime Single Page Apps with IHP

```toc

```

## Introduction

While IHP has a strong preference for server-side rendering, specific functionality in your app might require interactivity that can only be implemented with Javascript. In these cases we recommend that you follow a hybrid approach: Implement interactive functionality as a small single page app, while keeping unrelated functionality like the login and signup still server side rendered.

The hybrid approach gives you the best of both worlds: The low interactivity of a single page app where needed, while keeping the simplicity and high performance advantages of server side rendered IHP apps.

This guide will help you understand the best-practices of building hybrid applications with IHP and React.


### IHP Version

Please upgrade to IHP master to use DataSync.

Put this into your `default.nix`

```nix
# default.nix
let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "27eb7c65cbf57557af7fc712c93a1d49a5c63834";
    };
in
    ...
```

## Todo List App

### Adding React to your IHP project

#### Adding NodeJS to a project

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

#### The `Frontend` directory

Create a new `Frontend` directory. We'll place all JS source files in that directory in the next steps:

```bash
mkdir Frontend
````

#### NPM Init

Before we can install dependencies, we need to generate a `package.json`:

```bash
cd Frontend # npm needs to be run from the Frontend directory
npm init
```

Add the `Frontend/package.json` and the `Frontend/package.lock` to your git repository.

#### Installing React

Now we can just install node modules as required by calling `npm install`:

```bash
cd Frontend # npm needs to be run from the Frontend directory

# Install react and react-dom
npm add react react-dom

# We also need esbuild for bundling
npm add esbuild
```

#### Adding an Entrypoint

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

#### Adding Make Tasks

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

#### Running the Bundler

Open a new terminal tab and start the dev bundler:

```bash
make watch-frontend
```

This will recompile our `Frontend/app.jsx` whenever the file changes.

#### Mounting the React Component

Open `Web/Static/Welcome.hs` (or any other IHP view) and add this div somewhere to the view:

```haskell
<div id="hello-world"/>
```

Now open the view in the browser. You should see `Hello from react!` on the page.

### Building a Realtime SPA with IHP DataSync

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

#### Setting up the Schema

In this example we're going to build a simple todo list.

1. For that create a new table `todos` with the IHP Schema Designer. It needs the following structure:

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

    ![todos Table Schema](images/realtime-spas/todos-schema.png)

2. Run `Update DB` to insert the new table into your local database.

#### Adding the `ihp_authenticated` role

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

#### Enabling the DataSync Controllers

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

#### Loading the JS SDK

DataSync comes with a couple of JS files we need to load on our page before we can use it.

1. Open `Web/View/Layout.hs` and add these javascript files:

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

2. Remove the `helpers.js` from your `Web/View/Layout.hs`:

    Inside `Web/View/Layout.hs` you have a `<script src={assetPath "/helpers.js"}></script>`. Remove this line.
    The IHP JS helpers typically cause more troubles than necessary when working with react.

3. Open your IHP app and then run this in the chrome JS console:

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

#### Defining a Security Policy

1. Open the Schema Designer at `http://localhost:8001/`
2. Select the `todos` table, open the context menu and click `Add Policy`
    ![Context Menu with "Add Policy" selected](images/realtime-spas/add-policy.png)
3. You're now in the policy editor. Policies are a standard Postgresql feature. It's used by IHP to tell which user is allowed to see and edit your data.
    ![New Policy Editor](images/realtime-spas/new-policy-2.png)

    IHP detected that the `todos` table has a `user_id` column, and used this to auto-suggest the name and the fields of our policy:

    The `Visible if:` field contains `user_id = ihp_user_id()`. It tells postgres that a `todos` row is only visible to a user if `user_id = ihp_user_id()`. The `ihp_user_id()` is a database function provided by IHP that returns the user id of the logged in user of the IHP application.

    The `Additionally, allow INSERT and UPDATE only if:` field contains `user_id = ihp_user_id()` as well. This is a validation that is run for `INSERT` and `UPDATE` queries. It makes sure that a user can only `INSERT` todos for himself, and cannot change the user id with an `UPDATE` query to move the todo to someone else.

4. Click `Create Policy` to save the new policy
5. Run `Update DB` to apply the schema changes to your local dev db.

#### Fetching Data

1. Now open your local app again and try again to run the following JS snippet in the JS console:

    ```javascript
    await query('todos').fetch();
    ```

    ![](images/realtime-spas/js-console.png)

    This time it will output an empty array. That makes sense as we don't have any todos yet.

2. Let's add our first todo from JS land. Like on the backend side we can use the `createRecord` function. Type this into the JS console:

    ```javascript
    await createRecord('todos', { title: 'Hello World!', userId: '<PUT YOUR USER ID HERE>' })
    ```

    ![](images/realtime-spas/js-console-2.png)

3. Now that we've created a todo, let's try to fetch them again:

    ```javascript
    await query('todos').fetch();
    ```

    ![](images/realtime-spas/js-console-3.png)

    This returned a list with our todo in there.

    Like on the backend we can also use `fetchOne` here to only get returned a single `Todo` object instead of a `[Todo]`:
    
    ```javascript
    await query('todos').fetchOne();
    ```

    ![](images/realtime-spas/js-console-4.png)

    Now you should already have a good feeling for the basics of IHP DataSync. In the next steps we'll use `createRecord`, `updateRecord` and `fetch` to make a simple todo manager with react.



#### Implementing our Todo List

Let's first display our todo list:

1. Open `Frontend/app.jsx` and append this react component before `startApp`:

    ```javascript
    class TodoList extends React.Component {
        constructor(props) {
            super(props);
            this.state = { todos: null };
        }

        async componentDidMount() {
            const todos = await query('todos').fetch()
            this.setState({ todos });
        }

        render() {
            const { todos } = this.state;
            
            if (todos === null) {
                return <div className="spinner-border text-primary" role="status">
                    <span className="sr-only">Loading...</span>
                </div>;
            }

            return <div>
                {todos.map(todo => <div>{todo.title}</div>)}
            </div>
        }
    }
    ```
2. Next we want to render our `<TodoList/>` in the `<HelloWorld>` component. For that replace the `class HelloWorld` in `app.jsx` with this:
    ```javascript
    class HelloWorld extends React.Component {
        render() {
            return <div>
                <TodoList/>
            </div>
        }
    }
    ```
3. Open the app in the browser. The first `Hello World!` todo we've created in the previous step should be visible now:

    ![](images/realtime-spas/todo-1.png)


##### Adding Todos

Next we're going to add a todo form to create new todos:

1. Add this component to `app.jsx`:

    ```javascript
    class NewTodo extends React.Component {
        constructor(props) {
            super(props);
            this.state = { title: '' };
            this.handleSubmit = this.handleSubmit.bind(this);
        }

        render() {
            return <form onSubmit={this.handleSubmit} disabled={this.state.loading}>
                <div className="form-group d-flex flex-row">
                    <input
                        type="text"
                        className="form-control"
                        placeholder="New todo"
                        value={this.state.title}
                        onChange={event => this.setState({ title: event.target.value })}
                        disabled={this.state.loading}
                    />

                    <button type="submit" className="btn btn-primary" disabled={this.state.loading}>Save</button>
                </div>
            </form>
        }

        async handleSubmit(event) {
            const form = event.target;

            event.preventDefault();

            this.setState({ loading: true });
            await createRecord('todos', { title: this.state.title, userId: this.props.userId });

            this.setState({ loading: false, title: '' });
        }
    }
    ```

    This is mostly a conventional react form. The interesting part is inside the `handleSubmit`, where we call `await createRecord('todos', { title: this.state.title, userId: this.props.users });`.

2. Change the `render()` function of `HelloWorld` to also display our `<NewTodo/>` component:
    
    ```javascript
    class HelloWorld extends React.Component {
        render() {
            return <div>
                <TodoList/>
                <NewTodo userId={this.props.userId}/>
            </div>
        }
    }
    ```
3. We also need to pass the `userId` prop to the `HelloWorld` component. For that we're going to change the `startApp()` function to pass it via a data attribute from the haskell side to our JS frontend.

    Replace the `startApp` in `app.jsx` with this:

    ```javascript
    function startApp() {
        const appElement = document.getElementById('hello-world');
        ReactDOM.render(<HelloWorld userId={appElement.dataset.userId}/>, appElement);
    }
    ```

    Also open `Welcome.hs` (or any other haskell view where you're rendering the JS app) and add the data attribute for the `userId`:
    
    ```haskell
        instance View WelcomeView where
            html WelcomeView = [hsx|
                 <div
                    id="hello-world"
                    data-user-id={show (get #id currentUser)}
                />
            |]
    ```

    Now the user id is passed from the `hello-world` react root to the `<HelloWorld/>` component, and from there to our `<NewTodo/>` element.

4. Now you should be able to enter a new todo:
    
    ![](images/realtime-spas/new-todo.png)

    After you click the `Save` button, you need to manually refresh the page to make the new todo show up inside the `<TodoList/>` component. In the next section we will make this show up in realtime.


##### Making the `<TodoList/>` Realtime

To make our todos show up directly after adding them inside `<NewTodo/>`, we need to modify the `componentDidMount()` of the `TodoList`.

Right now it should like this:

```javascript
async componentDidMount() {
    const todos = await query('todos').fetch()
    this.setState({ todos });
}
```

Replace it with this:

```javascript
async componentDidMount() {
    await query('todos')
        .orderBy('createdAt')
        .fetchAndRefresh(todos => this.setState({ todos }))
}
```

You can see that instead of using `fetch` we now use `fetchAndRefresh` and pass a callback. The `todos => this.setState({ todos })` callback is called whenever the result set of our `query('todos')` we fired here changes. Our `todos => this.setState({ todos })` will then call `setState` which will trigger a re-render of our react component.

Open the app again and enter a new todo. You will now see it showing up instantly. You can also open a second browser window (keep in mind that you need to be logged in), and changes will appear in both windows at the same time.

##### Checking off Todos

Our todo list is a little incomplete without the ability to check things off.

First we need to add a new `TodoItem` component. For that add this function to the `app.jsx`:

```javascript
function TodoItem({ todo }) {
    const todoIdAttr = "todo-" + todo.id;

    return <div className="form-group form-check">
        <input
            id={todoIdAttr}
            type="checkbox"
            checked={todo.isCompleted}
            onChange={() => updateRecordById('todos', todo.id, { isCompleted: !todo.isCompleted })}
            className="mr-2"
        />
        <label className="form-check-label" htmlFor={todoIdAttr}>{todo.title}</label>
    </div>
}
```

This displays a checkbox next to the todo title. When the checkbox is toggled it will call `updateRecordById('todos', todo.id, { isCompleted: !todo.isCompleted })`. This function is similiar to the `updateRecord` on the Haskell side, but only takes a patch object as a argument.

Now we need to use this new `TodoItem` component inside our `TodoList`. Change the `render()` function of the `TodoList` to this:

```javascript
    render() {
        const { todos } = this.state;
        
        if (todos === null) {
            return <div className="spinner-border text-primary" role="status">
                <span className="sr-only">Loading...</span>
            </div>;
        }

        return <div>
            {todos.map(todo => <TodoItem todo={todo} key={todo.id}/>)}
        </div>
    }
```

Open the app again and try to check off a couple of todos.

##### Deleting Todos

To complete our todo list, we need to add a way to delete todos. For that we'll add a delete button to our `TodoItem`:

```javascript
function TodoItem({ todo }) {
    const todoIdAttr = "todo-" + todo.id;

    return <div className="form-group form-check">
        <!-- .. -->

        <button className="btn btn-link text-danger" onClick={() => deleteRecordById('todos', todo.id) }>Delete</button>
    </div>
}
````

##### Polishing

Our todo list is ready now. To make things look a little nicer you can optionally add this CSS to your `static/app.css`:

```css
.form-group .text-danger {
    opacity: 0;
}

.text-danger {
    margin-left: 1rem;
    transition: all 0.1s;
    font-size: 12px;
}

.form-group:hover .text-danger {
    opacity: 1;
}

label {
    user-select: none;
}
```

You have learned the basics of creating, updating and deleting database records while building a realtime app with IHP DataSync.

![](images/realtime-spas/todo-list-finished.png)

## Common DataSync Operations

The IHP DataSync JS SDK supports the following operations right now:

### Retrieving Records

#### Querying Records

You can retrieve all visible records of a table using query:

```javascript
const todos = await query('todos').fetch();

for (const todo of todos) {
    console.log(todo.title);
}
```

This will run a `SELECT * FROM todos` query and put a list of `Todo` structures.

#### Realtime Queries

To keep the result set in sync with the actual database state, use `fetchAndRefresh`:


```javascript
function callback(todos) {
    console.log('todos did change', todos);
}

const todos = await query('todos').fetchAndRefresh(callback);
```

The `fetchAndRefresh` function is using a websocket to be notified about any changes to the selected data set. [It's using IHP's `DataSubscription` API.](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/static/vendor/ihp-datasync.js) For more fine grained control you can use the `DataSubscription` API directly instead of relying on `fetchAndRefresh`.

#### Fetching a single record

When you have the id of a record, you can also use `fetchOne` to get it from the database:

```javascript
const todos = await query('todos')
        .filterWhere('id', 'd94173ec-1d91-421e-8fdc-20a3161b7802')
        .fetchOne();
````

This will run the SQL query `SELECT * FROM todos WHERE id = 'd94173ec-1d91-421e-8fdc-20a3161b7802' LIMIT 1`.

In case the record is not found, `fetchOne` will return `null`.

#### Order

Use `orderBy`, `orderByAsc` or `orderByDesc` to get a sorted result:

```javascript
const latestTodos = await query('todos')
        .orderByDesc('createdAt')
        .fetchOne();

const oldestTodos = await query('todos')
        .orderBy('createdAt') // 'orderBy' is an alias for 'orderByAsc'
        .fetchOne();
````

### Create Record

To insert a record into the database, call `createRecord` with a plain javascript object:

```javascript
createRecord(table, record)

// Example:
const newTodo = {
    title: 'Finish Guide',
    userId: '49946f4d-8a2e-4f18-a399-58e3296ecff5'
}
;
const insertedTodo = await createRecord('todos', newTodo);

console.log('id', insertedTodo.id);
```

### Create Many Record

You can use `createRecords` to insert multiple records with a single `INSERT` statement:

```javascript
createRecords(table, records)

// Example:
const todoA = { title: 'Finish Guide', userId: '49946f4d-8a2e-4f18-a399-58e3296ecff5' };
const todoB = { title: 'Learn Haskell', userId: '49946f4d-8a2e-4f18-a399-58e3296ecff5' };

const todos = await createRecord('todos', [ todoA, todoB ]);
```

### Update Record By ID

The function `updateRecordById` runs an `UPDATE` query for a specific record:

```javascript
updateRecordById(table, id, patch)

// Example:
const todo = await updateRecordById('todos', '66cc037e-5729-435c-b507-a17492fe44f4', { isCompleted: false });
```

### Delete Record By ID

Use `deleteRecordById` to run a simple `DELETE` query:

```javascript
deleteRecordById(table, id)

// Example:
await deleteRecordById('todos', '66cc037e-5729-435c-b507-a17492fe44f4');
```

This will execute:

```sql
DELETE FROM todos WHERE id = "66cc037e-5729-435c-b507-a17492fe44f4"
```


## Troubleshooting

`FormatError {fmtMessage = "1 single '?' characters, but 3 parameters", fmtQuery = "SELECT ? FROM ??", fmtParams = ["*","todos",""]}`

This error is caused by a bug in IHP v0.15 and IHP v0.16. You have two solution:
1. Upgrade to IHP v0.17 / latest IHP master.
2. Add a `orderBy('createdAt')` to your calls:

    ```javascript
    // FAILS with FormatError {fmtMessage = "1 single '?' characters, but 3 parameters", fmtQuery = "SELECT ? FROM ??", fmtParams = ["*","todos",""]}
    const todos = await query('todos').fetch();

    // GOOD
    const todos = await query('todos').orderBy('createdAt').fetch();
    ```

## Advanced IHP DataSync

### Advanced Policies

TODO