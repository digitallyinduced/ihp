{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Routes where

import IHP.RouterPrelude
import IHP.Router.DSL (routes)
import Generated.Types
import Web.Types

[routes|PostsController
GET    /Posts                  PostsAction
GET    /NewPost                NewPostAction
POST   /CreatePost             CreatePostAction
GET    /ShowPost?postId        ShowPostAction
GET    /EditPost?postId        EditPostAction
POST   /UpdatePost?postId      UpdatePostAction
DELETE /DeletePost?postId      DeletePostAction
|]
