{-|
Module: IHP.AuthSupport.Authorization
Description: Building blocks to provide authorization to your application
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.AuthSupport.Authorization where

import IHP.Prelude

class CanView user model where
    canView :: (?modelContext :: ModelContext) => model -> user -> IO Bool

-- | Stops the action execution with an error message when the access condition is True.
--
-- __Example:__ Checking a user is the author of a blog post.
-- 
-- > action EditPostAction { postId } = do
-- >     post <- fetch postId
-- >     accessDeniedWhen (post.authorId /= currentUserId)
-- >     
-- >     renderHtml EditView { .. }
--
-- This will throw an error and prevent the view from being rendered when the current user is not the author of the post.
accessDeniedWhen :: Bool -> IO ()
accessDeniedWhen condition = if condition then fail "Access denied" else pure ()

-- | Stops the action execution with an error message when the access condition is False.
--
-- __Example:__ Checking a user is the author of a blog post.
-- 
-- > action EditPostAction { postId } = do
-- >     post <- fetch postId
-- >     accessDeniedUnless (post.authorId == currentUserId)
-- >     
-- >     renderHtml EditView { .. }
--
-- This will throw an error and prevent the view from being rendered when the current user is not the author of the post.
accessDeniedUnless :: Bool -> IO ()
accessDeniedUnless condition = if condition then pure () else fail "Access denied"

