module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show

instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post
            |> orderByDesc #createdAt
            |> fetch
        render IndexView { .. }

    action NewPostAction = do
        let post = newRecord @Post
        render NewView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
            >>= fetchRelated #comments
        render ShowView { .. }

    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    redirectTo ShowPostAction { postId = post.id }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. }
                Right post -> do
                    post <- post |> createRecord
                    redirectTo PostsAction

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        redirectTo PostsAction

buildPost post = post
    |> fill @'["title", "body", "userId"]
    |> validateField #title nonEmpty
