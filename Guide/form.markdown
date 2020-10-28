# Form

```toc
```

## Introduction

In IHP Forms are an essential way to interact with your application. Dealing with a lot of form markup can quickly become complex because of the need to deal with consistent styling and especially when dealing with lots of validation. IHP provides helpers to generate form markup to help you deal with the complexity.

By default forms in IHP follow the class names used by Bootstrap 4. Therefore the forms work with Bootstrap 4 out of the box. Of course the default form generation can be customized to support other CSS frameworks.

Unless javascript helpers have been deactivated, your form will be submitted using AJAX and TurboLinks instead of a real browser based form submission.

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
        <input type="text" name="title" id="post_title" class="form-control">
    </div>

    <div class="form-group" id="form-group-post_body">
        <label for="post_body">Body</label>
        <textarea name="body" id="post_body" class="form-control"></textarea>
    </div>

    <button class="btn btn-primary">Create Post</button>
</form>
```

You can see that the form is submitted via `POST`. The form action has also been set by default to `/CreatePost`.

All inputs have auto-generated class names and ids for styling. Also all `name` attributes are set as expected.

## Form Controls

IHP has the most commonly-used form controls built in. In general the form control helpers just need to be passed the field name. Here is a list of all built-in form control helpers:

```html
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
{submitButton}
```

A form control is always filled with the value of the given field when rendering. For example given a post

```haskell
let post = Post { ..., title = "Hello World" }
```

Rendering `{textField #title}`, the input value will be set like

```html
<input ... value="Hello World">
```

## Validation

When rendering a record which has failed validation, the validation error message will be rendered automatically.

Given a post like this:


```haskell
let post = Post { ..., title = "" }
    |> validateField #title nonEmpty
```

Rendering `{textField #title}`, the input will have the css class `is-invalid` and an element with the error message will be rendered below the input:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>
    <input type="text" name="title" placeholder="" id="post_title" class="form-control is-invalid ">
    <div class="invalid-feedback">This field cannot be empty</div>
</div>
```

## Forms Are Also HSX

It's important to understand that while the form helpers like `{textField #title}` are called by `formFor`, you can still use HSX there. So you can just add any kind of HSX code inside your form:

```html
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
    </div>

    {textareaField #body}

    <div style="background: blue">
        {submitButton}
    </div>
|]
```

Inside the HSX block of a form, you have access to the special `?formContext` variable. This variable keeps track of e.g. the current record (`post` in the above example), the form action, as well as some other options. See the API Documentation on [`FormContext`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#t:FormContext) to learn more.

## Customizing Inputs

The return values of the form control helpers are usually a value of type [FormField](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#t:FormField). The `FormField` value is automatically rendered as HTML when used inside an HSX expression. Before this rendering happens, you can specifiy options to customize the rendering.

### Help Texts

You can add a help text below a form control like this:

```html
{(textField #title) { helpText = "Max. 140 characters"} }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="title" id="post_title" class="form-control">
    <small class="form-text text-muted">Max. 140 characters</small>
</div>
```

### Custom Field Label Text

By default the field name will be used as a label text. The camel case field name will be made more human readable of course, so `contactName` will turn to `Contact Name`, etc. Sometimes you want to change this auto-generated input label to something custom. Use `fieldLabel` for that, like this:

```html
{(textField #title) { fieldLabel = "Post Title"} }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Post Title</label>
    <input type="text" name="title" id="post_title" class="form-control">
</div>
```

### Custom CSS Classes

You can add custom CSS classes to the input and label for better styling. Set `fieldClass` for adding a class to the input element and `labelClass` for the label element:

```html
{(textField #title) { fieldClass="title-input", labelClass = "title-label" } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label class="title-label" for="post_title">Title</label>
    <input type="text" name="title" id="post_title" class="form-control title-input">
</div>
```

Of course, the CSS classes for validation are still set as expected.

### Placeholder

You can specify an input placeholder like this:

```html
{(textField #title) { placeholder = "Enter your title ..." } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="title" id="post_title" placeholder="Enter your title ..." class="form-control">
</div>
```

### Required Fields

You can mark an input as required like this:

```html
{(textField #title) { required = True } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="title" id="post_title" required="required" class="form-control">
</div>
```

### Custom Submit Button Text

Customize it like this:

```html
{submitButton { label = "Create it!" } }
```

This will render like:

```html
<button class="btn btn-primary">Create it!</button>
```

When you want to use e.g. an icon inside your button, it might be easier to just write the HTML manually by hand then.

### Custom Submit Button Class

Customize it like this:

```html
{submitButton { buttonClass = "create-button" } }
```

This will render like:

```html
<button class="btn btn-primary create-button">Create Post</button>
```


### Advanced Customization Options

The following options are not commonly used, but are useful sometimes.

#### Adding custom attributes to the input element

Form rendering is built on top of [blaze html](https://hackage.haskell.org/package/blaze-html). So you need to import the blaze html functions for this. Add this at the top of your module:

```haskell
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
```

```html
{(textField #title) { fieldInput = (\fieldInput -> H.input ! A.onclick "alert(1)") } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label class="" for="post_title">Title</label>
    <input onclick="alert(1)" type="text" name="title" placeholder="" id="post_title" class="form-control">
</div>
```

Using the `fieldInput`, which is passed as an argument, you can access the other options of the form. Don't set the `class` attribute on your custom field input, as this will break rendering.

#### Custom name attribute

By default the field name is used for the `name` attribute of the input element. You can override it like this:

```html
{(textField #title) { fieldName = "new-title" } }
```

This will render like:

```html
<div class="form-group" id="form-group-post_title">
    <label for="post_title">Title</label>

    <input type="text" name="new-title" id="post_title" class="form-control">
</div>
```

#### Custom id attribute

You can override the auto-generated id value like this:

```html
{(textField #title) { fieldInputId = "the-title-form-group" } }
```

This will render like:

```html
<div class="form-group" id="the-title-form-group">
    <label for="post_title">Title</label>

    <input type="text" name="new-title" id="post_title" class="form-control">
</div>
```

#### Don't render `<label>`

You can specifiy `disableLabel` to stop the label element from being generated:

```
{(textField #title) { disableLabel = True }
```

Will render as:

```html
<div class="form-group" id="form-group-post_title">
    <input type="text" name="title" placeholder="" id="post_title" class="form-control">
</div>
```

#### Don't render `<div class="form-group">`

You can specifiy `disableGroup` to stop the outer `<div class="form-group">` element from being generated:

```html
{(textField #title) { disableGroup = True }
```

Will render as:

```html
<input type="text" name="title" placeholder="" id="post_title" class="form-control">
<label for="post_title">Title</label>
```

#### Don't show validation error message

You can specify `disableValidationResult` to stop the validation error message being shown when the validation failed:

```html
{(textField #title) { disableValidationResult = True }
```


This works out of the box for most Haskell data types. When you are working with a custom data type, e.g. a custom enum value, you need to add a `InputValue MyDataType` implementation. We will cover this later in this Guide.

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
    -- Here we specify that the <option>-value should contain a UserId
    type SelectValue User = UserId
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
```

Given the above example, the rendered form will look like this:
```html
-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }]
<form ...>
    <select name="user_id">
        <option value="1">Marc</option>
        <option value="2">Andreas</option>
    </select>
</form>
```

### Select Inputs with Custom Enums

You can use select fields with custom defined enums too.

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


## Advanced Forms

You can get very far with the built-in form helpers. But sometimes you might need a very custom functionality which is not easily doable with the form helpers. In this case we highly recommend to not use the form helpers for that specific case. Don't fight the tools.

## CSRF

IHP by default sets its session cookies using the Lax [SameSite](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite) option. While `Lax` sounds not very secure, this protects against all common CSRF vectors. This browser-based CSRF protection works with all modern browsers, therefore a token-based protection is not used in IHP applications.

## Javascript Helpers

By default your form will be submitted using AJAX and [TurboLinks](https://github.com/turbolinks/turbolinks) instead of a real browser based form submission. It's implemented this way to support [SPA](https://en.wikipedia.org/wiki/Single-page_application)-like page transitions using TurboLinks and [morphdom](https://github.com/patrick-steele-idem/morphdom).

Additionally to integrate the form submission into TurboLinks, the javascript helpers will also disable the form submit button after the form has been submitted. Also any flash messages inside the form are removed.

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

TODO: This section still has to be implemented. The gist of how rendering can be completely overriden to support a different layout or CSS framework can be found in the implementation of [horizontalFormFor](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:horizontalFormFor) (renders a bootstrap 4 form in a horizontal way).
