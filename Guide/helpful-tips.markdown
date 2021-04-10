# Helpful Tips

## Special Haskell Syntax used by IHP

IHP uses many "exotic" haskell features. Here's a short explanation of the most common. In case you think something is missing, let us know on Slack.

### The hash symbols \#

IHP uses hash symbols `#` all over the place, like in this code:

```haskell
set #companyId (get #companyId company)
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

Inside actions the views are ususally rendered like this:

```haskell
action UsersAction = do
    users <- query @User |> fetch
    render IndexView { .. }
```

We asume that the view is defined like this:

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

Inside your `Types.hs` you see lots of `!`s, like this:

```haskell
ShowPostAction { postId :: !(Id Post) }
```

The `!` marks the `postId` field as strict. Strict means that the value of `postId` is not stored as a lazy value. So instead of only computing the `postId` field when needed, it will already be fully computed when the `ShowPostAction` value is constructed. Basically we tell haskell that we're sure that this field is always needed. This makes the action data structures more memory efficient.

[You can learn more about strictness and lazyness in haskell in this blog post.](https://www.fpcomplete.com/blog/2017/09/all-about-strictness/)

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

[Learn more about lambda cases here.](https://typeclasses.com/ghc/lambda-case)

### The Pipe operator `|>`

In IHP code bases you find lot's of usage of the `|>` operator. We usually call it the `pipe operator`.

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

[The operator is itself is defined as a haskell function inside `IHP.HaskellSupport`:](https://github.com/digitallyinduced/ihp/blob/master/IHP/HaskellSupport.hs#L56)

```haskell
infixl 8 |> -- This tells haskell to theat the |> as a infix operator
a |> f = f a -- This is the actuall implementation
```

## Tell GHC(Haskell Compiler) To Infer Constraints And Implicit Parameters

Let's say you are working with a controller `ApplicationsAction` and most actions have similar access control:

```haskell
    action NewApplicationAction { jobPositionId } = do
        jobPosition <- fetch jobPositionId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))

        ...

    action UpdateApplicationAction { applicationId } = do
        application <- fetch applicationId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))

        ...
```

We could start by refactoring the access control logic into a function:

```haskell
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
```

And then add a type declaration:

```haskell
accessDeniedUnlessJobPositionAllowed :: JobPosition -> IO ()
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
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
           accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
      In an equation for `accessDeniedUnlessJobPositionAllowed':
          accessDeniedUnlessJobPositionAllowed jobPosition
            = do jobPositions <- currentCompanyJobPositions
                 accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
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
