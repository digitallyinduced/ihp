# Authorization

```toc
```

## Restricting an action to logged in users

To restrict an action to a logged in user, use `ensureIsUser`:

```haskell
action PostsAction = do
    ensureIsUser
    posts <- query @Post |> fetch
    render IndexView { .. }
```

When someone is trying to access the `PostsAction` but is not logged in, the browser will be redirected to the login page. After the login succeeded, the user will be redirected back to the `PostsAction`.

It's common to restrict all actions inside a controller to logged in users only. Place the `ensureIsUser` inside the `beforeAction` hook to automatically apply it to all actions:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```

In this case `PostsAction` and `ShowPostAction` are only accessible to logged-in users.

## Restricting an action to logged in admins

To restrict an action to a logged in admin, use `ensureIsAdmin` instead of `ensureIsUser`. If you get

```
error:
    * Could not deduce (HasNewSessionUrl admin0)
        arising from a use of `ensureIsAdmin'
[…]
      These potential instance exist:
        instance HasNewSessionUrl Admin -- Defined in `Admin.Types'
```

then you may have to annotate the type with `@Admin`. For example:

```haskell
instance Controller UserController where
    beforeAction =
        ensureIsAdmin @Admin
```


## Checking for Permissions

You can use `accessDeniedUnless` to allow certain things only for specific users. For example, to restrict a `ShowPostAction` only to the user who a post belongs to, use this:

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (get #userId post == currentUserId)

        render ShowView { .. }
```
