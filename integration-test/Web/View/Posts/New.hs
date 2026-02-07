module Web.View.Posts.New where

import Web.View.Prelude

data NewView = NewView { post :: Post }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>New Post</h1>
        {renderForm post}
    |]

renderForm :: Post -> Html
renderForm post = formFor post [hsx|
    {textField #title}
    {textareaField #body}
    {submitButton}
|]
