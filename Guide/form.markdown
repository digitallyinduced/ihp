# Form

```toc

```

## Introduction

In IHP Forms are an essential way to interact with your application. Dealing with a lot of form markup can quickly become complex because of the need to deal with consistent styling and especially when dealing with lots of validation. IHP provides helpers to generate form markup to help you deal with the complexity.

By default forms in IHP follow the class names used by Bootstrap 4. Therefore the forms work with Bootstrap 4 out of the box. Of course, the default form generation can be customized to support other CSS frameworks.

Unless javascript helpers have been deactivated, your form will be submitted using AJAX and TurboLinks instead of browser-based form submission.

## Simple Forms

Forms usually begin with a `formFor` expression. This is how a simple form can look like:

```haskell
renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {textField #title}
    {textareaField #body}
    {submitButton}
|]
```

Calling this form from inside your HSX code will lead to the following HTML being generated:

```html
<form method="POST" action="/CreatePost" id="" class="new-form">
    <div class="form-group" id="form-group-post_title">
        <label for="post_title">Title</label>
        <input type="text" name="title" id="post_title" class="form-control" />
    </div>

    <div class="form-group" id="form-group-post_body">
        <label for="post_body">Body</label>
        <textarea name="body" id="post_body" class="form-control"></textarea>
    </div>

    <button class="btn btn-primary">Create Post</button>
</form>
```

You can see that the form is submitted via `POST`. The form action has also been set by default to `/CreatePost`.

All inputs have auto-generated class names and ids for styling. Also, all `name` attributes are set as expected.

## Form Controls

IHP has the most commonly-used form controls built in. In general the form control helpers just need to be passed the field name. Here is a list of all built-in form control helpers:

```haskell
{textField #title}
{textareaField #body}
{colorField #brandColor}
{emailField #email}
{dateField #dueAt}
{passwordField #password}
{dateTimeField #createdAt}
{numberField #quantity}
{hiddenField #projectId}
{checkboxField #termsAccepted}
{selectField #projectId allProjects}
{fileFile #profilePicture}
{submitButton}
```

A form control is always filled with the value of the given field when rendering. For example, given a post

```haskell
let post = Post { ..., title = "Hello World" }
```

Rendering `{textField #title}`, the input value will be set like

```html
<input ... value="Hello World" />
```

## Validation

When rendering a record that has failed validation, the validation error message will be rendered automatically.

Given a post like this:

```haskell
let post = Post { ..., title = "" }
    |> validateField #title nonEmpty
```

Rendering `{textField #title}`, the input will have the css class `is-invalid` and an element with the error message will be rendered below the input:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>
    <input
        type="text"
        name="title"
        placeholder=""
        id="post_title"
        class="form-control is-invalid "
    />
    <div class="invalid-feedback">This field cannot be empty</div>
</div>
```

## Forms Are Also HSX

It's important to understand that while the form helpers like `{textField #title}` are called by `formFor`, you can still use HSX there. So you can just add any kind of HSX code inside your form:

```haskell
renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    <h1>Add a new post</h1>

    <div class="row">
        <div class="col">
            {textField #title}
        </div>

        <div class="col">
            Specify a title at the left text field
        </div>

    {textareaField #body}

    <div style="background: blue">
        {submitButton}
    </div>
|]
```

Inside the HSX block of a form, you have access to the special `?formContext` variable. This variable keeps track of e.g. the current record (`post` in the above example), the form action, as well as some other options. See the API Documentation on [`FormContext`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#t:FormContext) to learn more.

## Customizing Inputs

The return values of the form control helpers are usually a value of type [FormField](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#t:FormField). The `FormField` value is automatically rendered as HTML when used inside an HSX expression. Before this rendering happens, you can specify options to customize the rendering.

### Help Texts

You can add a help text below a form control like this:

```haskell
{(textField #title) { helpText = "Max. 140 characters"} }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="title" id="post_title" class="form-control" />
    <small class="form-text text-muted">Max. 140 characters</small>
</div>
```

### Custom Field Label Text

By default, the field name will be used as a label text. The camel case field name will be made more human-readable of course, so `contactName` will turn to `Contact Name`, etc. Sometimes you want to change this auto-generated input label to something custom. Use `fieldLabel` for that, like this:

```haskell
{(textField #title) { fieldLabel = "Post Title"} }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Post Title</label>
    <input type="text" name="title" id="post_title" class="form-control" />
</div>
```

### Custom CSS Classes

You can add custom CSS classes to the input and label for better styling. Set `fieldClass` for adding a class to the input element and `labelClass` for the label element:

```haskell
{(textField #title) { fieldClass="title-input", labelClass = "title-label" } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label class="title-label" for="post_title">Title</label>
    <input
        type="text"
        name="title"
        id="post_title"
        class="form-control title-input"
    />
</div>
```

Of course, the CSS classes for validation are still set as expected.

### Placeholder

You can specify an input placeholder like this:

```haskell
{(textField #title) { placeholder = "Enter your title ..." } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input
        type="text"
        name="title"
        id="post_title"
        placeholder="Enter your title ..."
        class="form-control"
    />
</div>
```

### Required Fields

You can mark an input as required like this:

```haskell
{(textField #title) { required = True } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input
        type="text"
        name="title"
        id="post_title"
        required="required"
        class="form-control"
    />
</div>
```

### Disabled Fields

You can mark an input as disabled like this:

```haskell
{(textField #title) { disabled = True } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input
        type="text"
        name="title"
        id="post_title"
        disabled="disabled"
        class="form-control"
    />
</div>
```

### Autofocus

You can mark an input with autofocus, to ensure it will be given the input focus on page load, like this:

```haskell
{(textField #title) { autofocus = True } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input
        type="text"
        name="title"
        id="post_title"
        autofocus="autofocus"
        class="form-control"
    />
</div>
```


### Custom Submit Button Text

Customize it like this:

```haskell
{submitButton { label = "Create it!" } }
```

This will render like:

```html
<button class="btn btn-primary">Create it!</button>
```

When you want to use e.g. an icon inside your button, it might be easier to just write the HTML manually.

### Custom Submit Button Class

Customize it like this:

```haskell
{submitButton { buttonClass = "create-button" } }
```

This will render like:

```html
<button class="btn btn-primary create-button">Create Post</button>
```

### Advanced Customization Options

The following options are not commonly used, but are useful sometimes.

#### Adding custom attributes to the input element

Form rendering is built on top of [blaze html](https://hackage.haskell.org/package/blaze-html). So you need to import the blaze HTML functions for this. Add this at the top of your module:

```haskell
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
```

```haskell
{(textField #title) { fieldInput = (\fieldInput -> H.input ! A.onclick "alert(1)") } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label class="" for="post_title">Title</label>
    <input
        onclick="alert(1)"
        type="text"
        name="title"
        placeholder=""
        id="post_title"
        class="form-control"
    />
</div>
```

Using the `fieldInput`, which is passed as an argument, you can access the other options of the form. Don't set the `class` attribute on your custom field input, as this will break rendering.

#### Custom name attribute

By default the field name is used for the `name` attribute of the input element. You can override it like this:

```haskell
{(textField #title) { fieldName = "new-title" } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="new-title" id="post_title" class="form-control" />
</div>
```

#### Custom id attribute

You can override the auto-generated id value like this:

```haskell
{(textField #title) { fieldInputId = "the-title-form-group" } }
```

This will render like:

```html
<div class="form-group" id="the-title-form-group">
    <label for="post_title">Title</label>

    <input type="text" name="new-title" id="post_title" class="form-control" />
</div>
```

#### Don't render `<label>`

You can specify `disableLabel` to stop the label element from being generated:

```haskell
{(textField #title) { disableLabel = True }
```

Will render as:

```html
<div class="form-group" id="form-group-post_title">
    <input
        type="text"
        name="title"
        placeholder=""
        id="post_title"
        class="form-control"
    />
</div>
```

#### Don't render `<div class="form-group">`

You can specify `disableGroup` to stop the outer `<div class="form-group">` element from being generated:

```haskell
{(textField #title) { disableGroup = True }
```

Will render as:

```html
<input
    type="text"
    name="title"
    placeholder=""
    id="post_title"
    class="form-control"
/>
<label for="post_title">Title</label>
```

#### Don't show validation error message

You can specify `disableValidationResult` to stop the validation error message being shown when the validation failed:

```haskell
{(textField #title) { disableValidationResult = True }
```

This works out of the box for most Haskell data types. When you are working with a custom data type, e.g. a custom enum value, you need to add a `InputValue MyDataType` implementation. We will cover this later in this Guide.

### Standalone Validation Errors

If you're using a custom widget for a form field, you might still want to show IHP's validation errors. Use `validationResult #someField` to render a standalone validation error:

```haskell
formFor user [hsx|
    <div class="form-group">
        <label class="" for="passwordReset_email">Email</label>

        <!-- Custom widget here instead of using textField -->
        <input type="text" name="email" placeholder="" id="passwordReset_email" class="form-control is-invalid">

        {validationResult #email}
    </div>
|]
```

The `{validationResult #email}` will render like this when the validation failed:

```html
<div class="invalid-feedback">is not a valid email</div>
```

If there's no validation failure on the given field, the `validationResult` will not render anything.

#### Standalone Validation Errors Without Styling

If you need more control over the styling of your validation error, you can use `validationResultMaybe #someField`. This function will return `Just "some error"` when there's a validation failure on that field or `Nothing` if the field passed validation.

You can use this to write custom error messages in your form:

```haskell
formFor user [hsx|
    <div class="form-group">
        <label class="" for="passwordReset_email">Email</label>

        <!-- Custom widget here instead of using textField -->
        <input type="text" name="email" placeholder="" id="passwordReset_email" class="form-control is-invalid">

        {validationResultMaybe #email |> renderCustomError}
    </div>
|]

renderCustomError :: Maybe Text -> Html
renderCustomError (Just message) = [hsx|<p>validation failed: {message}</p>|]
renderCustomError Nothing = [hsx|all good|]
```

#### Adding Custom Errors

You can also add validation errors to a model manual, without using the IHP validation system:

```haskell
action NewInviteAction = do
    let invite = newRecord @Invite
            |> if isFreeUser
                then attachFailure #email "This feature requires you to have the paid plan of our product"
                else \user -> user

    render NewView { .. }
```


## Select Inputs

Select inputs require you to pass a list of possible values to select.

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
    -- Here we specify that the <option> value should contain a `Id User`
    type SelectValue User = Id User
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
```

Given the above example, the rendered form will look like this:

```html
<!-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }] -->
<form ...>
    <select name="user_id">
        <option value="1">Marc</option>
        <option value="2">Andreas</option>
    </select>
</form>
```

If you want a certain value to be preselected, set the value in the controller. For example, to have the first user be preselected in the above example:

```haskell
    action NewProjectAction = do
        users <- query @User |> fetch
        let userId = headMay users |> maybe def (get #id)
        let target = newRecord @Project |> set #userId userId
        render NewView { .. }
```

### Select Inputs with Nullable Value

Sometimes we want to allow the user to specifically make a choice of missing/none. To have our user-dropdown from the previous example allow this we need to adjust the `CanSelect` instance.

```haskell
instance CanSelect (Maybe User) where
    type SelectValue (Maybe User) = Maybe (Id User)
    selectValue (Just user) = Just (get #id user)
    selectValue Nothing = Nothing
    selectLabel (Just user) = get #name user
    selectLabel Nothing = "(none selected)"
```

Our select-helper also needs the list of options amended with the choice of `Nothing`. We add it as the first item in the list:

```haskell
formFor project [hsx|
    {selectField #userId (Nothing:(map Just users))}
|]
```

### Select Inputs with Custom Enums

You can use select fields with custom-defined enums too.

Given an enum like this:

```sql
CREATE TYPE CONTENT_TYPE AS ENUM ('video', 'article', 'audio');
```

We need to define a `CanSelect ContentType` like this:

```haskell
instance CanSelect ContentType where
    type SelectValue ContentType = ContentType
    selectValue value = value

    selectLabel Video = "Video"
    selectLabel Article = "Article"
    selectLabel Audio = "Audio"
    -- You can also use the following shortcut: selectLabel = tshow
```

The helper function `allEnumValues @ContentType` can then be used in your 
view to generate the list of select fields:

```haskell
  formFor subscription [hsx|
    {selectField #contentType allContentTypes}
|]
    where
      allContentTypes = allEnumValues @ContentType
```

### Select Inputs with Integers

It's a common use case to have a select field consisting of ints, e.g. inside a shopping cart to select the quantity of an item. 

The form can look like this:

```haskell
formFor subscription [hsx|
    {selectField #quantity quantities}
|]
    where
        quantities :: [Int]
        quantities = [1..10]
        -- Quick reminder: [1..10] is just a shortcut for [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] in haskell :)
```

You also need a `CanSelect` instance like this:


```haskell
instance CanSelect Int where
    type SelectValue Int = Int
    selectValue quantity = quantity
    selectLabel quantity = tshow quantity
```

## Customizing Forms

### Custom Form Action / Form URLs

The URL where the form is going to be submitted to is specified in HTML using the form's `action` attribute. When using `formFor` the `action` attribute is automatically set to the expected path.

E.g. given the below `formFor` code, the `action` is set to `/CreatePost` or `/UpdatePost`:

```haskell
renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {textField #title}
    {textareaField #body}
    {submitButton}
|]
```

To override the auto-generated `action` attribute use the `formFor'` function:

```haskell
renderForm :: Post -> Html
renderForm post = formFor' post "/my-custom-endpoint" [hsx||]
```

If you pass an action to that, you need to wrap it with `pathTo`:

```haskell
renderForm :: Post -> Html
renderForm post = formFor' post (pathTo CreateDraftAction) [hsx||]
```

If you want to combine this with other customizations, you can also specify a custom path using `formForWithOptions`:

```haskell
renderForm :: Post -> Html
renderForm post = formForWithOptions post options [hsx||]

options :: FormContext Post -> FormContext Post
options formContext =
    formContext
    |> set #formAction (pathTo CreateDraftAction)
    |> set #formClass "..." -- Other customizations
```

### Custom Form Class

By default forms have the CSS class `new-form` or `edit-form`, depending on if the record has been saved to the database yet:

```html
<form class="new-form">
<form class="edit-form">
```

You can override the form class using `formForWithOptions`:

```haskell
renderForm :: Post -> Html
renderForm post = formForWithOptions post options [hsx||]

options :: FormContext Post -> FormContext Post
options formContext =
    formContext
    |> set #formClass "custom-form-class"
```

The generated HTML will look like this:

```html
<form class="custom-form-class">
```

If you want to append your own classes while keeping the default `new-form` and `edit-form` classes, use `modify`:

```haskell
options :: FormContext Post -> FormContext Post
options formContext =
    formContext
    |> modify #formClass (\classes -> classes <> " custom-form-class")
```

### Custom Form Id

By default forms don't have an id. You can set a `<form id="">` attribute using `formForWithOptions`:

```haskell
renderForm :: Post -> Html
renderForm post = formForWithOptions post options [hsx||]

options :: FormContext Post -> FormContext Post
options formContext =
    formContext
    |> set #formId "post-form"
```

The generated HTML will look like this:

```html
<form id="post-form">
```

### Custom Form Attributes

You can specifiy custom HTML attributes using `formForWithOptions`.

```haskell
renderForm :: Post -> Html
renderForm post = formForWithOptions post options [hsx||]

options :: FormContext Post -> FormContext Post
options formContext =
        formContext
        |> set #customFormAttributes [ ("data-post-id", show postId) ]
    where
        post = get #model formContext
        postId = get #id post
```

The generated HTML will look like this:

```html
<form data-post-id="bd20f13d-e04b-4ef2-be62-64707cbda980">
```

### Disable Form Submission via JavaScript

Your form will be submitted using AJAX and TurboLinks instead of browser-based form submission.

Sometimes this behavior is problematic. For example when the successful form submissions redirects to page that starts a Single Page App. Usually you want to have a clean page refresh here to avoid troubles with the JavaScript.

Set `disableJavascriptSubmission` to `True` to use normal browser-based form submission:

```haskell
renderForm :: Post -> Html
renderForm post = formForWithOptions post options [hsx||]

options :: FormContext Post -> FormContext Post
options formContext =
    formContext 
    |> set #disableJavascriptSubmission True
```

There's also a shortcut called `formForWithoutJavascript` for this:

```haskell
renderForm :: Post -> Html
renderForm post = formForWithoutJavascript post [hsx||]
```

## Advanced Forms

You can get very far with the built-in form helpers. But sometimes you might need a very custom functionality which is not easily doable with the form helpers. In this case, we highly recommend not to use the form helpers for that specific case. Don't fight the tools.

### Multi Records in a Single Form

In some advanced cases you might want to have a single form that when submitted creates an arbitrary count of record. E.g. a form that creates multiple `posts` records.  When the form is submitted, each input should create its own record, so that if there are 60 inputs then 60 records should be made.

An action that can deal with an arbitrary amount of fields can look like this:

```haskell
    action CreatePostAction = do
        let titles :: [Text] = paramList "title"
        let bodys :: [Text] = paramList "body"

        let posts = zip titles bodys
                |> map (\(title, body) -> newRecord @Post
                        |> set #title title
                        |> set #body body
                        |> validateField #title nonEmpty
                        |> validateField #body nonEmpty
                    )

        validatedPosts :: [Either Post Post] <- forM posts (ifValid (\post -> pure post))

        case Either.partitionEithers validatedPosts of
            ([], posts) -> do
                createMany posts
                setSuccessMessage "Post created"
                redirectTo PostsAction

            (invalidPosts, validPosts) -> render NewView { posts }
```

The `NewView` needs to be changed as well to deal with an arbitrary amount of posts. For these cases we cannot use `formFor`, but we'll handle the job of `formFor` manually:

```haskell
module Web.View.Posts.New where
import Web.View.Prelude
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data NewView = NewView { posts :: [Post] }

instance View NewView where
    html NewView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
                <li class="breadcrumb-item active">New Post</li>
            </ol>
        </nav>
        <h1>New Post</h1>

        <form id="main-form" method="POST" action={CreatePostAction}>
            <input type="submit" class="btn btn-primary"/>
            {forEach posts renderForm}
        </form>
    |]

renderForm :: Post -> Html
renderForm post = [hsx|
    <div class="form-group">
        <label>
            Title
        </label>
        <input type="text" name="title" value={get #title post} class={classes ["form-control", ("is-invalid", isInvalidTitle)]}/>
        {titleFeedback}
    </div>

    <div class="form-group">
        <label>
            Body
        </label>
        <input type="text" name="body" value={get #body post} class={classes ["form-control", ("is-invalid", isInvalidBody)]}/>
        {bodyFeedback}
    </div>
|]
    where
        isInvalidTitle = isJust (getValidationFailure #title post)
        isInvalidBody = isJust (getValidationFailure #body post)

        titleFeedback = case getValidationFailure #title post of
            Just result -> [hsx|<div class="invalid-feedback">{result}</div>|]
            Nothing -> mempty

        bodyFeedback = case getValidationFailure #body post of
            Just result -> [hsx|<div class="invalid-feedback">{result}</div>|]
            Nothing -> mempty
```

We also need to make modifications to the `NewPostAction`:

```haskell
    action NewPostAction = do
        let post = newRecord
        let posts = take (paramOrDefault 2 "forms") $ repeat post
        render NewView { .. }
```

## CSRF

IHP by default sets its session cookies using the Lax [SameSite](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite) option. While `Lax` sounds not very secure, this protects against all common CSRF vectors. This browser-based CSRF protection works with all modern browsers, therefore token-based protection is not used in IHP applications.

## JavaScript Helpers

By default, your form will be submitted using AJAX and [TurboLinks](https://github.com/turbolinks/turbolinks) instead of browser-based form submission. It's implemented this way to support [SPA](https://en.wikipedia.org/wiki/Single-page_application)-like page transitions using TurboLinks and [morphdom](https://github.com/patrick-steele-idem/morphdom).

Additionally, to integrate the form submission into TurboLinks, the javascript helpers will also disable the form submit button after the form has been submitted. Also, any flash messages inside the form are removed.

When the IHP javascript helpers are included in a page, it will automatically hook into your form submissions. You can also call `window.submitForm(formElement)` to trigger a form submission from javascript.

The form helpers are designed to improve the User Experience for browsers where javascript is enabled. In case javascript is not enabled or blocked by a plugin, the form submission will still work as expected.

You can disable the form helpers by removing the IHP javascript helpers from your layout. In `Web/View/Layout.hs` remove the following line:

```html
<script src="/helpers.js"></script>
```

This way no special behavior will be attached to your forms.

To dig deeper into the javascript, [take a look at the source in helpers.js](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/static/helpers.js#L115).

## Working within the Bootstrap CSS framework

While the default forms layout is vertical with one field per line, it is easy to change. Bootstrap's excellent [forms documentation](https://getbootstrap.com/docs/4.4/components/forms/) shows how.

## Working with other CSS Frameworks

TODO: This section still has to be implemented. The gist of how rendering can be completely overridden to support a different layout or CSS framework can be found in the implementation of [horizontalFormFor](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:horizontalFormFor) (renders a bootstrap 4 form in a horizontal way).
