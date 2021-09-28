module IHP.ValidationSupport.Types where

import IHP.Prelude
import qualified Data.Text as Text
import IHP.ModelSupport (Violation (..))
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

data ValidatorResult
    = Success
    | Failure !Text
    | FailureHtml !Text
    deriving (Eq, Show)

{-# INLINE isSuccess #-}
isSuccess Success = True
isSuccess _       = False

{-# INLINE isFailure #-}
isFailure Failure {} = True
isFailure FailureHtml {} = True
isFailure _  = False

{-# INLINE attachValidatorResult #-}
attachValidatorResult :: (KnownSymbol field, HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> ValidatorResult -> model -> model
attachValidatorResult field Success record = record
attachValidatorResult field (Failure message) record = modify #meta prependAnnotation record
    where
        prependAnnotation :: MetaBag -> MetaBag
        prependAnnotation = modify #annotations (\a -> annotation:a)
        annotation = (Text.pack (symbolVal field), TextViolation message)
attachValidatorResult field (FailureHtml message) record = modify #meta prependAnnotation record
    where
        prependAnnotation :: MetaBag -> MetaBag
        prependAnnotation = modify #annotations (\a -> annotation:a)
        annotation = (Text.pack (symbolVal field), HtmlViolation message)

-- | Adds a plain-text validation error to a record
--
-- __Example:__
--
-- >>> record |> attachFailure #email "should be a valid email"
-- User { .., meta = MetaBag { .., annotations = [ ("email", TextViolation "should be a valid email") ] } }
--
-- You can use this together with 'getValidationFailure'
--
-- > user
-- >     |> attachFailure #email"cannot be empty"
-- >     |> getValidationFailure #email
-- >
-- > --  Returns: Just "cannot be empty"
--
-- If your error message uses HTML code, use 'attachFailureHtml'.
attachFailure :: (KnownSymbol field, HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> Text -> model -> model
attachFailure field !message = attachValidatorResult field (Failure message)
{-# INLINE attachFailure #-}

-- | Adds a validation error to a record. The error message can contain HTML code.
--
-- __Example:__
--
-- >>> record |> attachFailureHtml #email [hsx|should be a valid email. <a href="https://example.com/docs#email">Check out the documentation</a>|]
-- User { .., meta = MetaBag { .., annotations = [ ("email", HtmlViolation "should be a valid email. <a href="https://example.com/docs#email">Check out the documentation</a>") ] } }
--
-- You can use this together with 'getValidationViolation'
--
-- > user
-- >     |> attachFailure #email"cannot be empty"
-- >     |> getValidationViolation #email
-- >
-- > --  Returns: Just (HtmlViolation "should be a valid email. <a href="https://example.com/docs#email">Check out the documentation</a>")
attachFailureHtml :: (KnownSymbol field, HasField "meta" model MetaBag, SetField "meta" model MetaBag) => Proxy field -> Html -> model -> model
attachFailureHtml field !message = attachValidatorResult field (FailureHtml renderedHtml)
    where
        renderedHtml = message
                |> Blaze.renderHtml
                |> cs
{-# INLINE attachFailureHtml #-}

-- | Returns the validation failure for a field or Nothing
--
-- > user
-- >     |> attachFailure #email"cannot be empty"
-- >     |> getValidationFailure #email
-- >
-- > --  Returns: Just "cannot be empty"
--
-- When 'attachFailureHtml' is used, this function will return HTML code:
--
-- > user
-- >     |> attachFailureHtml #url [hsx|Invalid value, check <a href="https://example.com">the documentation</a>|]
-- >     |> getValidationFailure #url
-- >
-- > --  Returns: Just "Invalid value, check <a href="https://example.com">the documentation</a>"
--
--
-- If you need to special-case validation errors with HTML code, use 'getValidationViolation'
getValidationFailure :: (KnownSymbol field, HasField "meta" model MetaBag) => Proxy field -> model -> Maybe Text
getValidationFailure field model = get #message <$> getValidationViolation field model
{-# INLINE getValidationFailure #-}

-- | Similar to 'getValidationFailure', but exposes the information whether the error message contains HTML code
--
-- >>> user |> getValidationViolation #email
-- Just (TextViolation "cannot be empty")
--
getValidationViolation :: (KnownSymbol field, HasField "meta" model MetaBag) => Proxy field -> model -> Maybe Violation
getValidationViolation field model =
        model
            |> get #meta
            |> get #annotations
            |> lookup fieldName
    where
        fieldName = Text.pack (symbolVal field)