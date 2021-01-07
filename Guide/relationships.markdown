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

The type of `post` is `Include "comments" Post` instead of the usual `Post`. This way the state of a fetched nested resource is tracked at the type level.

### Order by

When we want to order the relationship in a certain way, we can just apply our commonly used `orderBy` function:

```haskell
let postId :: Id Post = ...

post <- fetch postId
    >>= pure . modify #comments (orderByDesc #createdAt)
    >>= fetchRelated #comments
```

This works because the `comments` field of a `Post` is just a `QueryBuilder` before we call `fetchRelated`.

This query builder is equivalent to:

```haskell
query @Comment |> filterWhere (#postId, get #id post)
```

The call to `>>= pure . modify #comments (orderByDesc #createdAt)` just appends a `|> orderByDesc #createdAt` like this:

```haskell
query @Comment |> filterWhere (#postId, get #id post) |> orderByDesc #createdAt
```

Then the `fetchRelated` basically just executes this query builder and puts the result back into the `comments` field of the `post` record.

### Multiple Records

When we want to fetch all the comments for a list of posts, we can use `collectionFetchRelated`:

```haskell
posts <- query @Post
    |> fetch
    >>= collectionFetchRelated #comments
```

This will query all posts with all their comments. The type of posts is `[Include "comments" Post]`.

The above Haskell code will trigger the following two SQL queries to be executed:

```sql
SELECT posts.* FROM posts
SELECT comments.* FROM comments WHERE post_id IN (?)
```

### Sorting With Multiple Records

If you want to sort the results after fetching multiple records with `collectionFetchRelated`

```haskell
posts <-
    query @Post |> fetch
        >>= collectionFetchRelated #comments
        >>= \posts ->
            posts
                |> sortOn (\post -> post |> get #comment |> get #title)
                |> pure
```

## Belongs To Relationships

Given a specific comment, we can fetch the post this comment belongs to. Like other relationships this is also using `fetchRelated`:

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

The type of `comment` is `Include "postId" Comment` instead of the usual `Comment`. This way the state of a fetched nested resource is tracked at the type level.

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
