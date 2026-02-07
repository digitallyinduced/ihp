module Web.View.Posts.Index where

import Web.View.Prelude

data IndexView = IndexView { posts :: [Post] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <h1>Posts</h1>
        <div>
            {forEach posts renderPost}
        </div>
    |]

renderPost :: Post -> Html
renderPost post = [hsx|
    <div class="post">
        <h2>{post.title}</h2>
        <p>{post.body}</p>
    </div>
|]
