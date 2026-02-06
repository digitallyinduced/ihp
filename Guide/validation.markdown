# Validation

```toc

```

## Introduction

IHP provides a simple infrastructure for validating incoming data. This guide will tell you more about validating new and existing records, as well as how to use more complex validations with database access.

## Quickstart

### Setting up the controller

Let's assume we have generated a `Posts` controller using the code generator. Our `Post` has a `title` and a `body`. The `CreatePostAction` looks like this:

```haskell
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. }
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction
```

This action is executed when a form like the one below is submitted:

```haskell
module Web.View.Posts.New where
import Web.View.Prelude

data NewView = NewView { post :: Post }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New Post</h1>
        {renderForm post}
    |]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {textField #title}
    {textField #body}
    {submitButton}
|]
```

Now let's add some validation.

### Adding Validation Logic

To make sure that the `title` and `body` are not empty, we can [`validateField ... nonEmpty`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateField) like this:

```haskell
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> validateField #title nonEmpty
            |> validateField #body nonEmpty
            |> ifValid \case
                Left post -> render NewView { post }
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

buildPost post = post
    |> fill @'["title", "body"]
```

The syntax to validate a record is always like this:

```haskell
record
    |> validateField #fieldName validatorName
```

### Common Validators

Here is a list of the most common validators:

Works with Text fields:
- [`|> validateField #name nonEmpty`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:nonEmpty)
- [`|> validateField #email isEmail`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:isEmail)
- [`|> validateField #phoneNumber isPhoneNumber`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:isPhoneNumber)

Works with ints:
- [`|> validateField #rating (isInRange (1, 10))`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:isInRange)

You can find [the full list of built-in validators in the API Documentation](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html).

### Validate `Maybe` Fields.

You can use all the existing validators with `Maybe` fields. The validator will only be applied when the field is not `Nothing`.

```haskell
buildPost :: Post -> Post
buildPost post = post
    |> validateField #title nonEmpty
    -- Assuming sourceUrl is optional.
    |> validateField #sourceUrl (validateMaybe nonEmpty)
```

### Fill Validation

When using [`fill`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:fill), like `|> fill @'["title", "body"]`, any error parsing the input is also added as a validation error.

E.g. when [`fill`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:fill) fills in an integer attribute, but the string `"hello"` is submitted, an error will be added to the record and the record is not valid anymore. The record attribute will then keep its old value (before applying [`fill`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:fill)) and later re-render in the error case of [`ifValid`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:ifValid). This applies to all arguments read via [`fill`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:fill). It's very helpful when dealing with strings expected in a certain format, like date times.

As IHP is never putting raw strings into the records, without previously parsing them, it does not provide validators like Rails's `:only_integer`.

### Rendering Errors

When a post with an empty title is now submitted to this action, an error message will be written into the record. The call to [`|> ifValid`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:ifValid) will see that there is an error, and then run the `Left post -> render NewView { post } `, which displays the form to the user again. The form will be rendered, and errors will automatically pop up next to the form field.

The default form helpers like [`{textField #title}`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:textField) automatically render the error message below the field. Here is how this looks:

![Validation Error Message Below Title Input](images/first-project/title_non_empty.png)

### Validating An Email Is Unique

For example, when dealing with users, you usually want to make sure that an email is only used once for a single user. You can use [`|> validateIsUnique #email`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:validateIsUnique) to validate that an email is unique for a given record.

This function queries the database and checks whether there exists a record with the same email value. The function ignores the current entity of course.

This function does IO, so any further arrows have to be [`>>=`](https://ihp.digitallyinduced.com/api-docs/IHP-Prelude.html#v:-62--62--61-), like this:

```haskell
action CreateUserAction = do
    let user = newRecord @User
    user
        |> fill @'["email"]
        |> validateIsUnique #email
        >>= ifValid \case
            Left user -> render NewView { .. }
            Right user -> do
                createRecord user
                redirectTo UsersAction
```

#### Case Insensitive Uniqueness

Usually emails like `someone@example.com` and `Someone@example.com` belong to the same person. You can use [`validateIsUniqueCaseInsensitive`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:validateIsUniqueCaseInsensitive) to ignore the case when checking for uniqueness:


```haskell
action CreateUserAction = do
    let user = newRecord @User
    user
        |> fill @'["email"]
        |> validateIsUniqueCaseInsensitive #email
        >>= ifValid \case
            Left user -> render NewView { .. }
            Right user -> do
                createRecord user
                redirectTo UsersAction
```

For good performance in production it's recommended to add an index on the column in your `Schema.sql`:

```sql
CREATE UNIQUE INDEX users_email_index ON users ((LOWER(email)));
```

### Sharing Between Create and Update Action

Usually, you have a lot of the same validation logic when creating and updating a record. To avoid duplicating the validation rules, you can apply them inside the `buildPost` function. This function is used by the create as well as the update action to read in the form values.

Here is how this can look:

```haskell
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            -- <------------ Here we removed the `validateField ...`
            |> ifValid \case
                Left post -> render NewView { post }
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

buildPost post = post
    |> fill @'["title", "body"]
    |> validateField #title nonEmpty -- <---------- Here we added them
    |> validateField #body nonEmpty
```

In case a validation should only be used for e.g. updating a record or creating a record, just keep it there in the action only and don't move it to the `buildPost` function.

### Creating a custom validator

You can just write your constraint like this:

```haskell
nonEmpty :: Text -> ValidatorResult
nonEmpty "" = Failure "This field cannot be empty"
nonEmpty _ = Success

isAge :: Int -> ValidatorResult
isAge = isInRange (0, 100)
```

Then just call it like:

```haskell
user |> validateField #age isAge`
```

### Creating a custom validator that uses IO

Use [`validateFieldIO`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateFieldIO) to validate a field based upon a value that is fetched from the database. The below example shows how to validate that a post’s `title` field contains a unique value.

```haskell
-- In Controller/Posts.hs or Helper/Controller.hs
-- Add custom validator function that checks if the post’s title is unique.
titleIsUnique :: (?modelContext :: ModelContext) => Post -> Text -> IO ValidatorResult
titleIsUnique post title = do
    exists <-
        query @Post
            |> filterWhere (#title, post.title)
            |> filterWhereNot (#id, post.id)
            |> fetchExists

    if exists
        then pure $ Failure "Title is not unique"
        else pure Success

-- In Controller/Posts.hs
-- Add type signature to buildPost function, specifying that it requires the Model Context and 
-- Controller Context to be available and that the function’s return value will be IO Post rather than Post.
buildPost :: (?modelContext :: ModelContext, ?context :: ControllerContext) => Post -> IO Post
buildPost post = post
    |> fill @'["title", "body"]
    -- Add custom validator.
    |> (\post -> validateFieldIO #title (titleIsUnique post) post) 
```

In your controller, wherever you use the `buildPost` function, since it is now inside IO, use the `>>=` (bind) operator rather than `|>` (pipe) operator after it. 

```haskell
post
    |> buildPost
    >>= ifValid \case -- Changed |> to >>=
        Left post -> do
            render NewView { .. } 
        Right post -> do
            post <- post |> createRecord
            setSuccessMessage "Post created"
            redirectTo PostsAction
```

### Checking If A Record Is Valid

Use [`ifValid`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:ifValid) to check for validity of a record:

```haskell
post |> ifValid \case
    Left post -> do
        putStrLn "The Post is invalid"
    Right post -> do
        putStrLn "The Post is valid"
```

This will call the `Left post -> do ...` block when the record is not valid.

You can also use it like this:

```haskell
let message = post |> ifValid \case
    Left post -> "The post is invalid"
    Right post -> "The post is valid"

putStrLn message
```

### Customizing Error Messages

#### Use [`withCustomErrorMessage`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:withCustomErrorMessage) to customize the error message when validation failed:

```haskell
user
    |> fill @'["firstname"]
    |> validateField #firstname (nonEmpty |> withCustomErrorMessage "Please enter your firstname")
```

In this example, when the [`nonEmpty`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:nonEmpty) adds an error to the user, the message `Please enter your firstname` will be used instead of the default `This field cannot be empty`.

#### Use [`withCustomErrorMessageIO`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:withCustomErrorMessageIO) to customize the error message when using IO functions:

```haskell
user
    |> fill @'["email"]
    |> withCustomErrorMessageIO "Email Has Already Been Used" validateIsUnique #email
    >>= ifValid \case
        Left user -> ...
        Right user -> ...
```

In this example, when the [`validateIsUnique`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateIsUnique.html#v:validateIsUnique) function adds an error to the user, the message `Email Has Already Been Used` will be used instead of the default `This is already in use`.

## Security Concerns and Conditional `fill`

It's important to remember that any kind of validations you might have on the form level are not enough to ensure the security of your application. You should always have validations on the backend as well. The user might manipulate the form data and send invalid data to your application.

So if a field is disabled or has an integer field with a min/max value, you should always have a validation on the backend.

Another point is that you don't have to always `fill` all fields in one go. Sometimes you'd like to conditionally `fill` based on the current user or based on the current logic.

Let's see those examples in action. Let's say we have a `Comment` record that has a `postId` that references a `Post`, a `body` field, and a moderation field allowing admin users to indicate if they are approved or rejected.

Here's an excerpt from the `Schema.sql`:

```sql
-- Schema.sql

-- Add a moderation status column to the comments table
CREATE TYPE comment_moderation AS ENUM ('comment_moderation_pending', 'comment_moderation_approved', 'comment_moderation_rejected');

CREATE TABLE comments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    post_id UUID NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
    comment_moderation comment_moderation NOT NULL
);
```

Once we generate a controller for the `Comment` record, we'll the `buildComment` function that is called from both the `CreateCommentAction` and the `UpdateCommentAction`:

```haskell
-- Controller/Comments.hs

buildComment comment = comment
    |> fill @'["postId", "body", "commentModeration"]
```

We'll start with the `postId`. Once a comment is refernecing a post it will never have the reference change. So it means we should fill it only upon creation.

```haskell
buildComment comment = comment
    |> fill @'["body", "commentModeration"]
    |> fillIsNew
    where
        fillIsNew record =
            if isNew record
            -- Record is new, so fill the `postId`.
            then fill @'["postId"] record
            -- Otherwise, leave the record as is.
            else record
```

Next, imagine we have a `currentUserIsAdmin` indicating if the current user is an admin. We'd like to allow only admins to set the moderation status of a comment. So we'll allow `fill` on the `commentModeration` field only in that case:

```haskell
-- A fake implementation of `currentUserIsAdmin`.
-- Will return true if the user's email is `admin@example.com`.
currentUserIsAdmin :: (?context :: ControllerContext) => Bool
currentUserIsAdmin =
    case currentUserOrNothing of
        Just user -> user.email == "admin@example.com"
        Nothing -> False

buildComment comment = comment
    |> fill @'["body"]
    |> fillIsNew
    |> fillCurrentUserIsAdmin
    where
        fillIsNew record =
            if isNew record
            -- Record is new, so fill the `postId`.
            then fill @'["postId"] record
            -- Otherwise, leave the record as is.
            else record

        fillCurrentUserIsAdmin record =
            if currentUserIsAdmin
            then fill @'["commentModeration"] record
            else record
```

Let's finish with a final example. Let's assume there was also a `score` integer field between 1 - 5 that only the admin could set. As mentioned, we'd need to have a validation on the backend to ensure that the user didn't manipulate the form data. And use `fill` to ensure that a non-admin user can't set the score in the first place. Here's the final code, where we conditionally `fill` the `score` field only if the user is an admin, and perform validation on it:

```haskell
buildComment comment = comment
    |> fill @'["body"]
    |> fillIsNew
    |> fillCurrentUserIsAdmin
    where
        fillIsNew record =
            if isNew record
            -- Record is new, so fill the `postId`.
            then fill @'["postId"] record
            -- Otherwise, leave the record as is.
            else record

        fillCurrentUserIsAdmin record =
            if currentUserIsAdmin
            then fill @'["commentModeration", "score"] record
                    -- Make sure that star can be only between 1 and 5.
                    |> validateField #score (isInRange (1, 5))
            else record


currentUserIsAdmin :: (?context :: ControllerContext) => Bool
currentUserIsAdmin =
    case currentUserOrNothing of
        Just user -> user.email == "admin@example.com"
        Nothing -> False
```


## Internals

IHP's validation is built with a few small operations.

### [`validateField`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateField)

The primary operation is `validateField #field validationFunction record`.

This function does the following thing:

1. Read the `#field` from the record
2. Apply the `validationFunction` to the field value
3. When the validator returns errors, store the errors inside the `meta` attribute of the record.

The [`validateField`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateField) function expects the `record` to have a field `meta :: MetaBag`. This `meta` field is used to store validation errors.

Let's say we have an example data type `Post`:

```haskell
data Post = Post { title :: Text, meta :: MetaBag }
```

A call to [`validateField`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateField) will result in the following:

```haskell
let post = Post { title = "" , meta = def } -- def stands for default :)

post |> validateField #title nonEmpty
-- This will return:
--
-- Post {
--     title = "",
--     meta = MetaBag {
--         annotations = [
--             ("title", "This field cannot be empty")
--         ]
--     }
-- }
```

As you can see, the errors are tracked inside the `MetaBag`. When you apply another [`validateField`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:validateField) to the record, the errors will be appended to the `annotations` list.

### Validation Functions

A validation function is just a function which, given a value, returns `Success` or `Failure "some error message"`.

Here is an example:

```haskell
isColor :: Text -> ValidatorResult
isColor text | ("#" `isPrefixOf` text) && (length text == 7) = Success
isColor text = Failure "is not a valid color"
```

Calling [`isColor "#ffffff"`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:isColor) will return `Success`. Calling [`isColor "something bad"`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html#v:isColor) will result in `Failure "is not a valid color"`.

It might be useful to take a look at the definition of some more validation functions to see how it works. [You can find them in the API Docs](https://ihp.digitallyinduced.com/api-docs/src/IHP.ValidationSupport.ValidateField.html#isColor).

### Attaching Errors To A Record Field

You can attach errors to a specific field of a record even when not validating. These errors will then also show up when rendering a form.

Here is an example:

```haskell
post
    |> attachFailure #title "This error will show up"
```

This record now has a validation error attached to its title.

#### Attaching Errors with HTML

If you try to use HTML code within [`attachFailure`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-Types.html#v:attachFailure), the HTML code will be escaped and not rendered as expected:

```haskell
post
    |> attachFailure #title "Invalid value. <a href="https://example.com/docs">Check the documentation</a>"
    -- Link will not be clickable as the HTML is escaped
```

Use [`attachFailureHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-Types.html#v:attachFailureHtml) instead:

```haskell
post
    |> attachFailureHtml #title [hsx|Invalid value. <a href="https://example.com/docs">Check the documentation</a>|]
    -- Link will work as expected, as it's HSX
```

### Retrieving The First Error Message For A Field

You can also access an error for a specific field using [`getValidationFailure`](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-Types.html#v:getValidationFailure):

```haskell
post
    |> validateField #name nonEmpty
    |> getValidationFailure #name
```

This returns `Just "Field cannot be empty"` or `Nothing` when the post has a title.

### Retrieving All Error Messages For A Record

Access them from the `meta :: MetaBag` attribute like this:

```haskell
record.meta.annotations
```

This returns a `[(Text, Violation)]`, e.g. `[("name", "This field cannot be empty")]`.
