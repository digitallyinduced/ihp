module Foundation.HaskellSupport ((|>), isEmpty, whenEmpty) where
    import ClassyPrelude
    import Control.Monad (when)
    import qualified Data.Default
    import qualified Data.UUID

    --(|>) :: a -> f -> f a
    infixl 8 |>
    a |> f = f a

    isEmpty :: (Eq a, Monoid a) => a -> Bool
    isEmpty value = value == mempty

    whenEmpty condition = when (isEmpty condition)


    instance Data.Default.Default Data.UUID.UUID where
        def = Data.UUID.nil
