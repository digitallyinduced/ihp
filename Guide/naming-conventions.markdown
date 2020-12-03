# Naming Conventions

```toc

```

## Introduction

The code of applications powered by IHP should always feel like it's the same kind of application. This helps to quickly switch between projects. Also, the end-user of your application does not care whether something uses camel-case or snake-case, but consistency is important in the project. Therefore IHP comes with a set of rules on how to name certain things.

## Database

Table names should always use lowercase, snake case and be in the plural form.

Here are some examples of good names:

```haskell
users
projects
companies
user_projects
company_admins
reactions
invites
comments
```

Here are some examples of bad names:

```haskell
post -- Not in plural form, should be `posts`
UserProjects -- Not in snake case, should be `user_projects`
Posts -- Should be lowercase `posts`
company -- Should be `companies`
```

## View

**Module Names:**

View modules should always follow this naming schema:

```haskell
module <app>.View.<controller>.<actionVerb>
```

The `<controller>` should not end in `Controller`. The `<actionVerb>` should not end in `View`.

Here are some examples of good names:

```haskell
module Web.View.Users.Show
module Web.View.Users.Edit
module Web.View.Companies.New
module Admin.View.Users.New
module Admin.View.UserProjects.Show
```

Here are some examples of bad names:

```haskell
module Web.View.PostsController.Show -- Should not end in "Controller"
module Web.View.Posts.EditView -- Should not end in "View"
module Web.View.Post.New -- Should most-likely be plural (unless the controller is called PostController)
```

**Data Structure:**

The `View` data structure should always end in `View`, as it's usually imported into the controller module unqualified.

Here are some examples of good data structures:

```haskell
data EditView = EditView
data NewView = NewView
data NewView = NewView
data IndexView = IndexView
data PostView = SimplePostView | AdvancedPostView
data QuestionTemplatesView = QuestionTemplatesView
```

Here are some examples of bad data structures:

```haskell
data Edit = Edit -- Missing the View suffix
data NewView = NewPost -- The view constructor should be named the same as the type (when only a single constructor)
```

## Controller

**Module Names:**

Controller modules should always follow this naming schema:

```haskell
module <app>.Controller.<controller>
```

The `<controller>` should not end in `Controller`. It should usually be in plural form unless it's only working on a single entity. E.g. you might have a `UserController` when only dealing with the current user because from the users point of view, there is only a single user resource they can interact with.

Here are some examples of good names:

```haskell
module Web.Controller.Companies
module Web.Controller.Users
module Web.Controller.Static
module Admin.Controller.Sessions
```

Here are some examples of bad names:

```haskell
module Web.Controller.PostsController -- Should not end in Controller
```

**Data Structure:**

The `Controller` data structure should always end in `Controller`, as it's usually imported into other modules unqualified.

Here are some examples of good data structures:

```haskell
data UsersController
    = UsersAction
    | NewUserAction
    | ShowUserAction { userId :: !(Id User) }
    | CreateUserAction
    | EditUserAction { userId :: !(Id User) }
    | UpdateUserAction { userId :: !(Id User) }
    | DeleteUserAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)

data CompaniesController
    = CompaniesAction
    | NewCompanyAction
    | ShowCompanyAction { companyId :: !(Id Company) }
    | CreateCompanyAction
    | EditCompanyAction { companyId :: !(Id Company) }
    | UpdateCompanyAction { companyId :: !(Id Company) }
    | DeleteCompanyAction { companyId :: !(Id Company) }
    deriving (Eq, Show, Data)

data AdminsController
    = AdminsAction
    | NewAdminAction
    | ShowAdminAction { adminId :: !(Id Admin) }
    | CreateAdminAction
    | EditAdminAction { adminId :: !(Id Admin) }
    | UpdateAdminAction { adminId :: !(Id Admin) }
    | DeleteAdminAction { adminId :: !(Id Admin) }
    deriving (Eq, Show, Data)

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)
```

Here are some examples of bad data structures:

```haskell
data CompanyController -- Type should be plural in this case
    = CompaniesAction
    | NewCompanyAction
    | ShowCompanyAction { companyId :: !(Id Company) }
    | CreateCompanyAction
    | EditCompanyAction { companyId :: !(Id Company) }
    | UpdateCompanyAction { companyId :: !(Id Company) }
    | DeleteCompanyAction { companyId :: !(Id Company) }
    deriving (Eq, Show, Data)

```
