module Foundation.HaskellSupport ((|>), isEmpty, whenEmpty) where
    import ClassyPrelude
    import Control.Monad (when)
    --(|>) :: a -> f -> f a
    a |> f = f a

    isEmpty :: (Eq a, Monoid a) => a -> Bool
    isEmpty value = value == mempty

    whenEmpty condition = when (isEmpty condition)