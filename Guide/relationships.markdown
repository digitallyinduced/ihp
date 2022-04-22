# Relationships

```toc

```

## Introduction

The following sections assume the following database schema being given. It's the same as in "Your First Project".

```sql
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

CREATE TABLE comments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    post_id UUID NOT NULL,
    author TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);

ALTER TABLE comments ADD CONSTRAINT comments_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE CASCADE;
```

## Has Many Relationships

Given a specific post, we can fetch the post and all its comments like this:

```haskell
let postId :: Id Post = ...

post <- fetch postId
    >>= fetchRelated #comments
```

This Haskell code will trigger the following SQL queries to be executed:

```sql
SELECT posts.* FROM posts WHERE id = ?  LIMIT 1
SELECT comments.* FROM comments WHERE post_id = ?
```

In the view we can just access the comments like this:

```haskell
[hsx|
    <h1>{get #title post}</h1>
    <h2>Comments:</h2>
    {post |> get #comments}
|]
```

The `post |> get #comments` returns a list of the comments belonging to the post.

The type of `post` is [`Include "comments" Post`](https://ihp.digitallyinduced.com/api-docs/IHP-MailPrelude.html#t:Include) instead of the usual `Post`. This way the state of a fetched nested resource is tracked at the type level.

It is possible to have multiple nested resources. For example, if Post had a list of comments and tags related to it, it can be defined as [`Include "comments" (Include "tags" Post)`](https://ihp.digitallyinduced.com/api-docs/IHP-MailPrelude.html#t:Include) or with the more convinient way as [`Include' ["comments", "tags"] Post`](https://ihp.digitallyinduced.com/api-docs/IHP-MailPrelude.html#t:Include-39-).

Note that for the above example, it is expected that the query will change as-well:

```haskell
let postId :: Id Post = ...

post <- fetch postId
    >>= fetchRelated #comments
    >>= fetchRelated #tags
```

### Order by

When we want to order the relationship in a certain way, we can just apply our commonly used [`orderBy`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:orderBy) function:

```haskell
let postId :: Id Post = ...

post <- fetch postId
    >>= pure . modify #comments (orderByDesc #createdAt)
    >>= fetchRelated #comments
```

This works because the `comments` field of a `Post` is just a [`QueryBuilder`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#t:QueryBuilder) before we call [`fetchRelated`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:fetchRelated).

This query builder is equivalent to:

```haskell
query @Comment |> filterWhere (#postId, get #id post)
```

The call to `>>= pure . modify #comments (orderByDesc #createdAt)` just appends a [`|> orderByDesc #createdAt`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:orderByDesc) like this:

```haskell
query @Comment |> filterWhere (#postId, get #id post) |> orderByDesc #createdAt
```

Then the [`fetchRelated`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:fetchRelated) basically just executes this query builder and puts the result back into the `comments` field of the `post` record.

### Multiple Records

#### Fetching all Posts with their Comments (One-to-many)

When we want to fetch all the comments for a list of posts, we can use [`collectionFetchRelated`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:collectionFetchRelated):

```haskell
posts <- query @Post
    |> fetch
    >>= collectionFetchRelated #comments
```

This will query all posts with all their comments. The type of `posts` is [`[Include "comments" Post]`](https://ihp.digitallyinduced.com/api-docs/IHP-MailPrelude.html#t:Include).

The above Haskell code will trigger the following two SQL queries to be executed:

```sql
SELECT posts.* FROM posts
SELECT comments.* FROM comments WHERE post_id IN (?)
```

Inside the view you can access the comments like this:

```haskell
render = [hsx|
    <h1>Posts</h1>
    {forEach posts renderPost}
|]

renderPost :: Include "comments" Post -> Html
renderPost post = [hsx|
    <h2>{get #title post}</h2>
    {forEach comments renderComment}
|]
    where
        comments = get #comments post

renderComment :: Comment -> Html
renderComment comment = [hsx|
    <div class="comment">{get #body comment}</div>
|]
```

#### Fetching all Comments with their Posts (Many-to-one)

When we want to fetch all comments and display them with their posts, we need to do the reverse of the above:

```
comments <- query @Comment
    |> fetch
    >>= collectionFetchRelated #postId
```

This will query all comments and their respective posts. The type of `comments` is [`[Include "postId" Comment]`](https://ihp.digitallyinduced.com/api-docs/IHP-MailPrelude.html#t:Include).

The Haskell code will trigger the following two SQL queries:

```sql
SELECT comments.* FROM comments
SELECT posts.* FROM posts WHERE id IN (?)
```

Inside the view you can access the comment's post like this:

```haskell
render = [hsx|
    <h1>Comments</h1>
    {forEach comments renderComment}
|]

renderComment :: Include "postId" Comment -> Html
renderComment comment = [hsx|
    <h2>{get #title post}</h2>
    <div class="comment">{get #body comment}</div>
|]
    where
        -- The post is stored inside the postId field of the comment
        post = get #postId comment
```


### Order With Multiple Records

If you want to sort the results after fetching multiple records with [`collectionFetchRelated`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:collectionFetchRelated)

```haskell
posts <-
    query @Post
        |> fetch
        >>= pure . map (modify #comments (orderBy #createdAt))
        >>= collectionFetchRelated #comments
```

## Belongs To Relationships

Given a specific comment, we can fetch the post this comment belongs to. Like other relationships this is also using [`fetchRelated`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:fetchRelated):

```haskell
let comment :: Id Comment = ...

comment <- fetch comment
    >>= fetchRelated #postId
```

This Haskell code will trigger the following SQL queries to be executed:

```sql
SELECT comments.* FROM comments WHERE id = ? LIMIT 1
SELECT posts.* FROM posts WHERE id = ?  LIMIT 1
```

In the view we can just access the comments like this:

```haskell
[hsx|
    <h1>Comment to {comment |> get #postId |> get #title}</h1>
    <h2>Comments:</h2>
    {comment |> get #body}
|]
```

The type of `comment` is [`Include "postId" Comment`](https://ihp.digitallyinduced.com/api-docs/IHP-FetchRelated.html#v:fetchRelated) instead of the usual `Comment`. This way the state of a fetched nested resource is tracked at the type level.

## Delete Behavior

Usually, all your relations are secured at the database layer by using foreign key constraints. But that means e.g. deleting a post will fail when there still exists comments.

By default, a new foreign key constraint created via the Schema Designer has no `on delete` behavior specified. Therefore the foreign key constraint will look like this:

```sql
ALTER TABLE comments ADD CONSTRAINT comments_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE NO ACTION;
```

See the `NO ACTION` at the end of the statement? We have to change this do `CASCADE` to delete all comments when the related post is going to be deleted:

```sql
ALTER TABLE comments ADD CONSTRAINT comments_ref_post_id FOREIGN KEY (post_id) REFERENCES posts (id) ON DELETE CASCADE;
```

Of course, you can change this using the Schema Designer by clicking on the foreign key next to the `post_id` column in the `comments` table.

## Joins

It is possible to join tables to a given primary table (the one associated with the queried type) and use the joined table to select rows from the primary table. For instance, the following code could be used to retrieve all posts by users from department 5:

```haskell
query @Posts
        |> innerJoin @User (#authorId, #id)
        |> innerJoinThirdTable @Department @User (#id, #departmentId)
        |> filterWhereJoinedTable @Department (#number, 5)
```

[`innerJoin`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:innerJoin) is used to join the `users` table (for type `User`) to the primary table `posts` (for type `Posts`) on the columns `posts.author_id` and `users.id`. Type checks ascertain that both tables actually have the pertinent columns.

The function [`innerJoinThirdTable`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:innerJoinThirdTable) is used to join a third table on a column of some previously joined table. In the example, the table is `departments` and it is joined on `departments.id = users.department_id`. Again, the type system ascertains that the columns actually exist on the pertinent tables. It is furthermore ascertained that the table associated with the second type `User` has been joined before.

To add `WHERE` clauses involving a joined table, there is a family of functions of functions named like the ordinary filter functions, but suffixed with "JoinedTable". Where the normal filter functions use columns from the primary table, the tabel that the JoinedTable-functions operate on is specified by the type they are called with. In the example, the [`filterWhereJoinedTable`](https://ihp.digitallyinduced.com/api-docs/IHP-QueryBuilder.html#v:filterWhereJoinedTable) filters all rows where `department.number` equals 5.

### Many-to-many relationships and labeled results

Joins are also useful when it comes to many-to-many relationships. An example is the realationship between blog posts and tags: each post can have multiple tags and each tag can be attached to any number of posts. The following code could be used to obtain all posts with the tag 'haskell' or 'ihp'.

```haskell
query @Posts
        |> innerJoin @Tagging (#id, #postId)
        |> innerJoinThirdTable @Tag @Tagging (#id, #tagId)
        |> filterWhereInJoinedTable @Tag (#tagText, ["haskell", "ihp"])
        |> fetch
```

In the above example, the relationship between tags and posts will be lost after executing the query and it is impossible to find out, from the list of results alone, which post bears which tag. The function labelResults can be used to make this relationship transparent. The following code could be used to obtain a list of all posts together with the ids of the tags they are attached to.

```haskell
labeledComments <-
   query @Post
      |> innerJoin @Tagging (#id, #postId)
      |> innerJoinThirdTable @Tag @Tagging (#id, #tagId)
      |> labelResults @Tag #id
      |> fetch
```

`labeledComments` will be a list of objects of type LabeledData:

```haskell
data LabeledData a b = LabeledData { labelValue :: a, contentValue :: b }
```

In the case above, `a` would be instantiated by (Id' "tags") and `b` by `Post`.

### Simple Joins and Outer Joins
An alternative approach to joining data in IHP can be accomplished by using the [postresql-simple (:.)](https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple-Types.html#t::.)
and a custom sql query.

For example say there is a `Student`, `StudentDeskCombo`, and `Desk` data type derived by IHP from
`students`, `student_desk_combos`, and `desks` tables.

If the application wished to get a list of all the desks and whether a student
is associated with that desk a `left outer join` on the three tables would be a simple
way of accomplishing this. The postgresql data type `(:.)` allows for a compound data
structure to be created without having to define any `newtype` wrappers or define
functions that do any type level computations.

All that is required is that a `FromRow` instance for any potentially nullable return value in the query, e.g. `Maybe Student`,
is manually defined in the IHP application:

```haskell
instance FromRow (Maybe Student) where
    fromRow =  (null *> null *> null *> pure Nothing) <|> (Just <$> fromRow)
       where null = field :: RowParser Null
```

At the moment the postgresql-simple library does not derive this instance generically.

Once you define this instance, preferably in `Application.Helper.Controller`, you can then
access the IHP derived data types directly by writing a custom sql query:

```
deskStudentCombos :: [Desk :. Maybe StudentDeskCombo :. Maybe Student] <- sqlQuery [select * from desks
                      left outer join on studentdeskcombo.desk_id = desks.id
                      left outer join on studentdeskcombo.student_id = students.id
                      ]()
```


the result data type can be unpacked and rendered using straight forward pattern matching with the `(:.)`
data type/type constructor:
```
renderStudentDesk :: (Desk :. Maybe StudentDeskCombo :. Student) -> Html
renderStudentDesk (desk :. Just studentDeskCombo :. Just student) = [hsx|{get #name student} {get #id desk}|]
renderStudentDesk (desk :. Nothing :. Nothing) = [hsx|<p>No student assigned to this Desk:  {get #id desk}.</p>|]
```

In the case of inner joins the process is even simpler and does not require
defining the `instance FromRow Maybe a`. This approach to joins allows
for custom queries to leverage the autogenerated schema/IHP derived data types directly
and cuts down on clutter from `newtype` definitions.



### Many-to-many relationships and views

Let's say we have the following schema:

```
posts:
- id

tags:
- id
- name

- posts_tags:
- id
- post_id
- tag_id
```

We want to display a list of all posts with their tags.

We can use it like this:

```haskell
action PostsAction = do
    posts <- query @Post |> fetch

    postsTags <- query @PostTag
        |> filterWhereIn (#postId, ids posts)
        |> fetch

    tags <- query @Tag
        |> filterWhereIn (#id, map (get #tagId) postsTags)
        |> fetch

    render PostsView { .. }
```

In our view we can now render the posts like this:
```haskell
html PostsView { .. } = [hsx|
    {forEach posts renderPost}
|]
    where
        renderPost post = [hsx|
            {post}
            {forEach thisTags renderTag}
        |]
            where
                thisTags :: [Tag]
                thisTags = postsTags
                    |> filter (\postTag -> get #postId postTag == get #id post)
                    |> mapMaybe (\postTag -> find (\tag -> get #id tag == get #tagId postTag) tags)

        renderTag tag = [hsx|
            <span>{get #name tag}</span>
        |]
```
