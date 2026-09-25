# Helpful Tips

## Special Haskell Syntax used by IHP

IHP uses many "exotic" haskell features. Here's a short explanation of the most common. In case you think something is missing, let us know on Slack.

### The hash symbols \#

IHP uses hash symbols `#` all over the place, like in this code:

```haskell
set #companyId company.id
```

The hashes are provided by the [`OverloadedLabels` language extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html).

In IHP you can think of these hash-strings as immutable type-level strings. One of the most common uses for labels is to represent field names. In the above example `#companyId` refers to the `companyId` field on the `Company` data structure.

**Technical details:**

Writing `#companyId` is equivalent to writing `fromLabel @"companyId"`. The `fromLabel` function is provided by a type class and can be implemented by e.g. libraries and frameworks. In IHP this instance is usually this:

```haskell
instance IsLabel name (Proxy name') where
    fromLabel = Proxy @name'
```

So `#companyId` can be written as `fromLabel @"companyId"` which IHP turns into `Proxy @"companyId"`. The `Proxy` value is now a normal haskell value and is passed to functions such as `get` or `set`.

### The dot notation \.

IHP uses dot notation like `someRecord.someField` everywhere. You might not think of this as anything special, but this syntax was just recently added to Haskell.

The dot notation is provided by the [`OverloadedRecordDot` language extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html).

In IHP v0.19 and older IHP versions, the dot notation was not supported yet. So you might see `get #someField someRecord` instead in older projects. The `get #someField someRecord` notation is equivalent to `someRecord.someField` and is still supported:

```haskell
-- IHP v0.19 and before:
get #title project

-- IHP v0.20 and up:
project.title
```

### The at symbol @

Another symbol used often in IHP is the `@` symbol, like this:

```haskell
action UsersAction = do
    users <- query @User |> fetch
    render IndexView { .. }
```

This is called a Type application.

[There's a great blog post by IHP contributor Zac Wood explaining Type applications in IHP. It's best to just read the article :)](https://zacwood.me/posts/haskell-type-application/)


*If you have open questions about lists like `fill @["title", "body"]`, after reading the linked blog post:*

These are type-level lists. You can write `@"type level strings"` and type level integers like `@1337`, so you can also write type level lists like `@["title", "body"]`.

**Be aware:** When you write a type level list like `@["title"]`, so with only a single element, you need to prepend a `'` like this `@'["title"]`. Otherwise you'll get an error. [We opened a ticket on the haskell compiler for this already.](https://gitlab.haskell.org/ghc/ghc/-/issues/19096)

### The `{ .. }`

Inside actions the views are usually rendered like this:

```haskell
action UsersAction = do
    users <- query @User |> fetch
    render IndexView { .. }
```

We assume that the view is defined like this:

```haskell
data IndexView = IndexView { users :: [User] }
```


Then the expression `IndexView { .. }` is a shortcut for `IndexView { users = users }`.

When the haskell compiler expands this, it will internally be like this:

```haskell
action UsersAction = do
    users <- query @User |> fetch
    render IndexView { users = users }
```

[You can learn more here about these so-called `Record wildcards`.](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html)

### The bang operator `!`

Inside your `Web/Types.hs` you see lots of `!`s, like this:

```haskell
ShowPostAction { postId :: !(Id Post) }
```

The `!` marks the `postId` field as strict. Strict means that the value of `postId` is not stored as a lazy value. So instead of only computing the `postId` field when needed, it will already be fully computed when the `ShowPostAction` value is constructed. Basically we tell haskell that we're sure that this field is always needed. This makes the action data structures more memory efficient.

[You can learn more about strictness and laziness in haskell in this blog post.](https://www.fpcomplete.com/blog/2017/09/all-about-strictness/)

### The `\case`

Inside controllers you typically find code like this:

```haskell
user
    |> fill @["firstname", "lastname"]
    |> validateField #firstname nonEmpty
    |> ifValid \case
        Left user -> render EditView { .. }
        Right user -> do
            user <- user |> updateRecord
            redirectTo EditUserAction { .. }
```

The `\case` is called a `Lambda case` in haskell. It's a shortcut for writing this:

```haskell
user
    |> fill @["firstname", "lastname"]
    |> validateField #firstname nonEmpty
    |> ifValid (\user -> case user of
            Left user -> render EditView { .. }
            Right user -> do
                user <- user |> updateRecord
                redirectTo EditUserAction { .. }
        )
```

In general the `\case ...` can be expanded to `\value -> case value of ...`.

The lambda case is provided by the [`LambdaCase` language extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html)

[Learn more about lambda cases here.](https://typeclasses.com/ghc/lambda-case)

### The Pipe operator [`|>`](https://ihp.digitallyinduced.com/api-docs/IHP-HaskellSupport.html#v:-124--62-)

In IHP code bases you find lots of usage of the [`|>`](https://ihp.digitallyinduced.com/api-docs/IHP-HaskellSupport.html#v:-124--62-) operator. We usually call it the `pipe operator`.

The operator allows you to write code like this:

```haskell
user
    |> fill @["firstname", "lastname"]
    |> validateField #firstname nonEmpty
```

Instead of the normal function style:

```haskell
validateField #firstname nonEmpty (
        fill @["firstname", "lastname"] user
    )
```

In general:
```haskell
function arg1 arg2 object
=
object |> function arg1 arg2
```

[The operator itself is defined as a haskell function inside `IHP.HaskellSupport`:](https://github.com/digitallyinduced/ihp/blob/master/IHP/HaskellSupport.hs#L56)

```haskell
infixl 8 |> -- This tells haskell to treat the |> as an infix operator
a |> f = f a -- This is the actual implementation
```

## Tell GHC (Haskell Compiler) To Infer Constraints And Implicit Parameters

Let's say you are working with a controller `ApplicationsAction` and most actions have similar access control:

```haskell
    action NewApplicationAction { jobPositionId } = do
        jobPosition <- fetch jobPositionId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))

        ...

    action UpdateApplicationAction { applicationId } = do
        application <- fetch applicationId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))

        ...
```

We could start by refactoring the access control logic into a function:

```haskell
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))
```

And then add a type declaration:

```haskell
accessDeniedUnlessJobPositionAllowed :: JobPosition -> IO ()
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))
```

However, GHC will give us an error message stating:

```
...

Application/Helper/Controller.hs:51:21: error:
    * Unbound implicit parameter (?context::ControllerContext)
        arising from a use of `currentCompanyJobPositions'
    * In a stmt of a 'do' block:
        jobPositions <- currentCompanyJobPositions
      In the expression:
        do jobPositions <- currentCompanyJobPositions
           accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))
      In an equation for `accessDeniedUnlessJobPositionAllowed':
          accessDeniedUnlessJobPositionAllowed jobPosition
            = do jobPositions <- currentCompanyJobPositions
                 accessDeniedUnless (jobPosition.id `elem` (ids jobPositions))
   |
51 |     jobPositions <- currentCompanyJobPositions
   |
```

We could explicitly add the `?context::ControllerContext` implicit:

```haskell
accessDeniedUnlessJobPositionAllowed :: (?context::ControllerContext) => JobPosition -> IO ()
```

Writing out implicit parameters, and other type constrains could become messy and/or irritating, so we could tell GHC to infer it:

```haskell
accessDeniedUnlessJobPositionAllowed :: _ => JobPosition -> IO ()
```

## Essential Haskell Concepts for IHP

The sections below explain fundamental Haskell concepts that you will encounter in every IHP application. They are written for developers coming from languages like JavaScript, Python, or Ruby.

### Pattern Matching

Pattern matching lets you branch on the shape of a value. If you are familiar with `switch` or destructuring in JavaScript, pattern matching is similar but more powerful because the compiler checks that you have covered all cases.

#### Matching in function arguments

In IHP, every controller is a big pattern match. Each action is a different "shape" that the controller can receive:

```haskell
instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }

    action CreatePostAction = do
        ...
```

Each `action` clause matches a different data constructor. The `{ postId }` part extracts the `postId` field from the `ShowPostAction` value, so you can use it directly in the body.

#### case expressions

You can also pattern match in the middle of a function using `case ... of`:

```haskell
action ShowTaskAction { taskId } = do
    task <- fetch taskId
    case task.status of
        "open"   -> render OpenTaskView { .. }
        "closed" -> render ClosedTaskView { .. }
        _        -> render DefaultTaskView { .. }
```

The `_` is a wildcard that matches anything. It acts as a default/fallback case.

#### Matching on True/False

```haskell
case isAdmin of
    True  -> renderAdmin
    False -> redirectTo AccessDeniedAction
```

#### Matching on lists

```haskell
case users of
    []           -> putStrLn "No users found"
    [singleUser] -> putStrLn ("Found exactly one user: " <> singleUser.name)
    _            -> putStrLn "Found multiple users"
```

#### Common mistake: non-exhaustive patterns

If you forget to handle a case, the compiler will warn you. For example, if you match on `True` but forget `False`, your program could crash at runtime. Always include a wildcard `_` as a fallback when matching on values with many possibilities, or handle every constructor explicitly.

### Maybe, Just, and Nothing

In JavaScript or Python, any value can be `null` or `None`. This leads to the famous "null reference" bugs. Haskell avoids this by making "absence" explicit with the `Maybe` type.

A `Maybe` value is either:

- `Just x` -- there is a value, and it is `x`
- `Nothing` -- there is no value

Think of it like a box that either contains something or is empty.

#### Where you encounter Maybe in IHP

When a database column can be `NULL`, the generated Haskell type is wrapped in `Maybe`. For example, given this schema:

```sql
CREATE TABLE tasks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    assigned_user_id UUID
);
```

The `title` field becomes `Text` (always present), but `assigned_user_id` becomes `Maybe (Id User)` (might be `NULL`).

#### Handling Maybe values

**Using case (most explicit):**

```haskell
case task.assignedUserId of
    Just userId -> do
        user <- fetch userId
        putStrLn ("Assigned to: " <> user.name)
    Nothing -> putStrLn "Unassigned"
```

**Using `fromMaybe` (provide a default):**

```haskell
let name = fromMaybe "Anonymous" user.nickname
-- If user.nickname is Just "Alice", name is "Alice"
-- If user.nickname is Nothing, name is "Anonymous"
```

**Using `maybe` (transform or default):**

```haskell
let greeting = maybe "No user" (\name -> "Hello, " <> name) user.nickname
```

**Using `isJust` and `isNothing` (just check):**

```haskell
when (isNothing task.assignedUserId) do
    putStrLn "Warning: task is unassigned"
```

#### Common mistake: using a Maybe value directly

If you write `fetch task.assignedUserId` when `assignedUserId` is `Maybe (Id User)`, you will get a type error. The compiler is telling you: "this might be `Nothing`, you need to handle that case first." Use `fetchOneOrNothing` instead, or unwrap the `Maybe` with a `case` expression.

### The |> Pipe Operator

The pipe operator `|>` passes a value into a function. It works like the Unix pipe `|` -- the result of the left side becomes the input to the right side.

```haskell
x |> f      -- is the same as:  f x
x |> f |> g -- is the same as:  g (f x)
```

IHP uses `|>` extensively for building queries and processing records. It reads top-to-bottom, like a pipeline:

```haskell
action PostsAction = do
    posts <- query @Post
        |> filterWhere (#isPublished, True)
        |> orderByDesc #createdAt
        |> limit 10
        |> fetch
    render IndexView { .. }
```

Without `|>`, the same code would use nested function calls that read inside-out:

```haskell
posts <- fetch (limit 10 (orderByDesc #createdAt (filterWhere (#isPublished, True) (query @Post))))
```

The pipe version is much easier to read and modify.

#### |> vs $

You will also see the `$` operator in Haskell code. It works in the opposite direction -- it passes the right side into the function on the left:

```haskell
putStrLn $ "Hello, " <> name
-- is the same as:
putStrLn ("Hello, " <> name)
```

Think of `$` as "apply what follows." It is mainly used to avoid parentheses.

In summary:

- `|>` pipes left-to-right: `value |> function`
- `$` applies right-to-left: `function $ value`

### do Notation

In languages like JavaScript, you write sequential steps one after another. In Haskell, you use `do` notation to do the same thing. Every IHP action is a `do` block:

```haskell
action CreatePostAction = do
    let post = newRecord @Post
    post <- post
        |> fill @'["title", "body"]
        |> createRecord
    setSuccessMessage "Post created"
    redirectTo PostsAction
```

Each line in a `do` block runs in order, just like sequential code in other languages.

#### The <- arrow (bind)

The `<-` arrow extracts a value from an action that "does something" (like a database query or reading a request parameter). Think of it as `await` in JavaScript:

```haskell
-- Haskell with do notation:
user <- fetch userId

-- Conceptually similar to JavaScript:
-- const user = await fetch(userId)
```

Use `<-` when the right side is an IO action (database query, network request, etc.).
Use `let` when the right side is a pure computation (no side effects).

```haskell
action ShowPostAction { postId } = do
    post <- fetch postId                      -- <- because fetch does IO (database query)
    let title = post.title                    -- let because accessing a field is pure
    let uppercaseTitle = toUpper title         -- let because toUpper is pure
    render ShowView { .. }
```

#### The last line is the result

The last line of a `do` block is its result. You do not need to write `pure` or `return` explicitly in most IHP actions, because the last line is usually `render` or `redirectTo`, which already produce the right type.

#### Common mistake: forgetting <-

If you write:

```haskell
action ShowPostAction { postId } = do
    let post = fetch postId  -- WRONG: this stores the action, not the result
    render ShowView { .. }
```

The compiler will give you a type error. The fix is to use `<-` instead of `let`:

```haskell
action ShowPostAction { postId } = do
    post <- fetch postId  -- CORRECT: this runs the query and gives you the result
    render ShowView { .. }
```

The rule of thumb: if the function talks to the database, reads from the request, or has any side effect, use `<-`. If it is a pure calculation, use `let`.

### let and where

Haskell provides two ways to define local variables: `let` and `where`.

#### let inside do blocks

Inside a `do` block, `let` works like variable assignment in other languages:

```haskell
action CreateUserAction = do
    let user = newRecord @User
    user <- user
        |> fill @'["name", "email"]
        |> createRecord
    let welcomeMessage = "Welcome, " <> user.name <> "!"
    setSuccessMessage welcomeMessage
    redirectTo UsersAction
```

Note: `let` in a `do` block does not use `in`. It is just `let x = ...` on its own line.

#### where at the end of a function

`where` lets you define helper values or functions at the end, keeping the main logic at the top:

```haskell
action ShowDashboardAction = do
    posts <- query @Post
        |> filterWhere (#authorId, currentUserId)
        |> fetch
    let stats = computeStats posts
    render DashboardView { .. }
    where
        computeStats posts =
            let total = length posts
                published = length (filter (\p -> p.isPublished) posts)
            in (total, published)
```

Use `let` for quick inline definitions. Use `where` when you want to keep the main function body clean and define helpers afterward.

### Type Signatures

A type signature tells you what a function accepts and what it produces. Reading type signatures is one of the most useful skills for working with IHP.

#### Basic syntax

```haskell
functionName :: InputType -> OutputType
```

Read `::` as "has type" and `->` as "takes ... and returns ...".

```haskell
toUpper :: Text -> Text
-- "toUpper takes a Text and returns a Text"

fetch :: Id User -> IO User
-- "fetch takes an Id User and returns an IO User"
-- (IO means it does something with side effects, like a database query)

filterWhere :: (field, value) -> query -> query
-- "filterWhere takes a tuple of (field, value) and a query, and returns a query"
```

#### Multiple arguments

Functions with multiple arguments have multiple `->` arrows:

```haskell
set :: field -> value -> record -> record
-- "set takes a field, a value, and a record, and returns a record"
-- Example: set #name "Alice" user
```

#### Checking types in GHCi

When you are unsure about a type, use `:t` (short for `:type`) in GHCi:

```
ghci> :t fetch
fetch :: (...) => id -> IO result
```

This is very helpful for debugging type errors.

#### Common type error patterns

When you see an error like:

```
Couldn't match type 'Maybe Text' with 'Text'
```

It means you are passing a `Maybe Text` (a value that might be absent) where a plain `Text` is expected. You need to unwrap the `Maybe` first (see the Maybe section above).

### Implicit Parameters (?modelContext, ?context)

In most web frameworks, things like the database connection or the current request are available globally or passed via dependency injection. IHP uses a Haskell feature called implicit parameters for this.

#### What are implicit parameters?

An implicit parameter is a value that is automatically passed through function calls without you having to write it out. You can recognize them by the `?` prefix:

- `?modelContext` -- the database connection (needed for queries)
- `?context` -- the controller context (request, session, flash messages, framework config)

#### You usually do not need to think about them

Inside controller actions, views, and most IHP code, these implicit parameters are already available. You can just call `fetch`, `query`, `currentUser`, and other framework functions without worrying about where the database connection comes from:

```haskell
action PostsAction = do
    -- ?modelContext is automatically available here
    -- so you can just call fetch directly:
    posts <- query @Post |> fetch
    render IndexView { .. }
```

#### When they matter: extracting helper functions

Implicit parameters become visible when you extract code into a helper function. If your helper calls database functions, it needs `?modelContext` in its type:

```haskell
-- This helper queries the database, so it needs ?modelContext
fetchPublishedPosts :: (?modelContext :: ModelContext) => IO [Post]
fetchPublishedPosts = query @Post
    |> filterWhere (#isPublished, True)
    |> orderByDesc #createdAt
    |> fetch
```

If your helper accesses the request or session, it needs `?context`:

```haskell
-- This helper accesses the session, so it needs ?context
isAdmin :: (?context :: ControllerContext) => Bool
isAdmin = currentUser.role == "admin"
```

#### The shortcut: let GHC infer constraints

Instead of writing out the full constraint, you can use `_` to let the compiler figure it out:

```haskell
fetchPublishedPosts :: _ => IO [Post]
```

This is covered in the "Tell GHC to Infer Constraints" section above.

#### Understanding implicit parameter errors

When you see an error like:

```
Unbound implicit parameter (?modelContext::ModelContext)
  arising from a use of 'fetch'
```

It means your function calls something that needs a database connection, but the type signature does not declare this requirement. The fix is to add the constraint to your type signature:

```haskell
-- Before (causes the error):
myHelper :: IO [Post]

-- After (fixes it):
myHelper :: (?modelContext :: ModelContext) => IO [Post]

-- Or let GHC infer it:
myHelper :: _ => IO [Post]
```
