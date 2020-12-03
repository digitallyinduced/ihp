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

To make sure that the `title` and `body` are not empty, we can `validateField ... nonEmpty` like this:

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

```haskell
-- works with Text fields
|> validateField #name nonEmpty
|> validateField #email isEma
|> validateField #phoneNumber isPhoneNumber

-- works with ints
|> validateField #rating (isInRange 1 10)
```

You can find [the full list of built-in validators in the API Documentation](https://ihp.digitallyinduced.com/api-docs/IHP-ValidationSupport-ValidateField.html).

### Fill Validation

When using `fill`, like `|> fill @'["title", "body"]`, any error parsing the input is also added as a validation error.

E.g. when `fill` fills in an integer attribute, but the string `"hello"` is submitted, an error will be added to the record and the record is not valid anymore. The record attribute will then keep its old value (before applying `fill`) and later re-render in the error case of `ifValid`. This applies to all arguments read via `fill`. It's very helpful when dealing with strings expected in a certain format, like date times.

As IHP is never putting raw strings into the records, without previously parsing them, it does not provide validators like Rails's `:only_integer`.

### Rendering Errors

When a post with an empty title is now submitted to this action, an error message will be written into the record. The call to `|> ifValid` will see that there is an error, and then run the `Left post -> render NewView { post } `, which displays the form to the user again. The form will be rendered, and errors will automatically pop up next to the form field.

The default form helpers like `{textField #title}` automatically render the error message below the field. Here is how this looks:

![Validation Error Message Below Title Input](images/first-project/title_non_empty.png)

### Validating An Email Is Unique

For example, when dealing with users, you usually want to make sure that an email is only used once for a single user. You can use `|> validateIsUnique #email` to validate that an email is unique for a given record.

This function queries the database and checks whether there exists a record with the same email value. The function ignores the current entity of course.

This function does IO, so any further arrows have to be `>>=`, like this:

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

### Checking If A Record Is Valid

Use `ifValid` to check for validity of a record:

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

Use `withCustomErrorMessage` to customize the error message when validation failed:

```haskell
user
    |> fill @'["firstname"]
    |> validateField #firstname (nonEmpty |> withCustomErrorMessage "Please enter your firstname")
```

In this example, when the `nonEmpty` adds an error to the user, the message `Please enter your firstname` will be used instead of the default `This field cannot be empty`.

## Internals

IHP's validation is built with a few small operations.

### `validateField`

The primary operation is `validateField #field validationFunction record`.

This function does the following thing:

1. Read the `#field` from the record
2. Apply the `validationFunction` to the field value
3. When the validator returns errors, store the errors inside the `meta` attribute of the record.

The `validateField` function expects the `record` to have a field `meta :: MetaBag`. This `meta` field is used to store validation errors.

Let's say we have an example data type `Post`:

```haskell
data Post = Post { title :: Text, meta :: MetaBag }
```

A call to `validateField` will result in the following:

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

As you can see, the errors are tracked inside the `MetaBag`. When you apply another `validateField` to the record, the errors will be appended to the `annotations` list.

### Validation Functions

A validation function is just a function which, given a value, returns `Success` or `Failure "some error message"`.

Here is an example:

```haskell
isColor :: Text -> ValidatorResult
isColor text | ("#" `isPrefixOf` text) && (length text == 7) = Success
isColor text = Failure "is not a valid color"
```

Calling `isColor "#ffffff"` will return `Success`. Calling `isColor "something bad"` will result in `Failure "is not a valid color"`.

It might be useful to take a look at the definition of some more validation functions to see how it works. [You can find them in the API Docs](https://ihp.digitallyinduced.com/api-docs/src/IHP.ValidationSupport.ValidateField.html#isColor).

### Attaching Errors To A Record Field

You can attach errors to a specific field of a record even when not validating. These errors will then also show up when rendering a form.

Here is an example:

```haskell
post
    |> attachFailure #title "This error will show up"
```

This record now has a validation error attached to its title.

### Retrieving The First Error Message For A Field

You can also access an error for a specific field using `getValidationFailure`:

```haskell
post
    |> validateField #name nonEmpty
    |> getValidationFailure #name
```

This returns `Just "Field cannot be empty"` or `Nothing` when the post has a title.

### Retrieving All Error Messages For A Record

Access them from the `meta :: MetaBag` attribute like this:

```haskell
record
    |> get #meta
    |> #annotations
```

This returns a `[(Text, Text)]`, e.g. `[("name", "This field cannot be empty")]`.
