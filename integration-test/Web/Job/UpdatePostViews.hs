module Web.Job.UpdatePostViews where

import Web.Controller.Prelude

instance Job UpdatePostViewsJob where
    perform UpdatePostViewsJob { .. } = do
        post <- fetch postId
        post
            |> set #viewsCount (post.viewsCount + 1)
            |> updateRecord
        pure ()
