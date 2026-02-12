module Web.View.Posts.Show where

import Web.View.Prelude

data ShowView = ShowView { post :: Include "comments" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>
        <div>
            {forEach post.comments renderComment}
        </div>
    |]

renderComment :: Comment -> Html
renderComment comment = [hsx|
    <div class="comment">
        <p>{comment.body}</p>
    </div>
|]
