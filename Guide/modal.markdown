# Modal

```toc
```

## Introduction

IHP provides support to render [Bootstrap 4 Modals](https://getbootstrap.com/docs/4.5/components/modal/) out of the box. Here's how the result can look like:

![](images/modal/modal.png)

## Layout Setup

Before we can build our modal, we have to make sure that your current application layout has modal rendering enabled.

Open your `Web/View/Layout.hs` and add a `{modal}` just before the closing `</body>` tag:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<body>
    {inner}
    {modal}
</body>
|]
```

In case there is a `{modal}` already you can skip this step. Don't add the `{modal}` twice.

## Rendering Modal Views

Let's say we have a `ProjectsAction` displaying a list of projects. We have a `NewProjectAction` that you can use to add projects.

```haskell
data ProjectsController
    = ProjectsAction
    | NewProjectAction
```

Let's turn the `NewProjectAction` into a modal that renders on top of the projects list.

First we need to modify our `NewProjectAction`. It currently looks like this:

```haskell
action NewProjectAction = do
    let project = newRecord
    render NewView { .. }
```

We need to call `setModal NewView { .. }` to turn the `NewView` into a modal. We also need to call `jumpToAction ProjectsAction` to render the `NewView` on top of the `ProjectsView` which is going to be rendered by the `ProjectsAction`:

```haskell
action NewProjectAction = do
    let project = newRecord
    setModal NewView { .. }

    jumpToAction ProjectsAction
```

Let's take a look at `/NewPost`, it now looks like this:

![](images/modal/modal-2.png)

The `NewView` is now rendered inside the `ProjectsView` (it's rendered where the `{modal}` is placed). Next, we're going to add the modal styling.

## Modal Styling

The `Web/View/Projects/New.hs` view looks like this at the moment:

```haskell
module Web.View.Projects.New where
import Web.View.Prelude

data NewView = NewView { project :: Project }

instance View NewView where
    html NewView { .. } = [hsx|
        <div class="container">
            <nav>
                <ol class="breadcrumb">
                    <li class="breadcrumb-item"><a href={ProjectsAction}>Projects</a></li>
                    <li class="breadcrumb-item active">New Project</li>
                </ol>
            </nav>
            <h1>New Project</h1>
            {renderForm project}
        </div>
    |]
```

We're going to use `renderModal` inside the `html NewView { .. }` definition to turn this into a bootstrap-styled Modal view:

```haskell
module Web.View.Projects.New where
import Web.View.Prelude

data NewView = NewView { project :: Project }

instance View NewView where
    html NewView { .. } = renderModal Modal
                { modalTitle = "New Project"
                , modalCloseUrl = pathTo ProjectsAction
                , modalFooter = Nothing
                , modalContent = [hsx|
                        {renderForm project}
                    |]
                }
```

After that our `/NewProject` view looks like this:

![](images/modal/modal.png)

The call to `renderModal Modal { .. }` returns the HTML code for the bootstrap modal. You can think of it as a template function where `modalTitle`, `modalCloseUrl`, etc. just fill in the placeholder variables for the modal.
