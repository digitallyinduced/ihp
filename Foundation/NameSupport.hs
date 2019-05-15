module Foundation.NameSupport (tableNameToModelName, columnNameToFieldName, pluralToSingular, humanize, ucfirst, lcfirst, fieldNameToColumnName) where

import           ClassyPrelude
import           Data.String.Conversions (cs)
import qualified Text.Inflections        as Inflector
import qualified Data.Text

-- `users` => `User`
-- `projects` => `Project`
{-# INLINE tableNameToModelName #-}
tableNameToModelName :: Text -> Text
tableNameToModelName tableName = unwrapEither tableName $ Inflector.toCamelCased True $ cs (pluralToSingular tableName)

-- `email` => `email`
-- `project_id` => `projectId`
{-# INLINE columnNameToFieldName #-}
columnNameToFieldName :: Text -> Text
columnNameToFieldName columnName = unwrapEither columnName $ Inflector.toCamelCased False columnName

{-# INLINE unwrapEither #-}
unwrapEither _ (Right value) = value
unwrapEither input (Left value) = error "Foundation.NameSupport: " <> tshow value <> " (value to be transformed: " <>  tshow input <> ")"

-- `email` => `email`
-- `projectId` => `project_id`
{-# INLINE fieldNameToColumnName #-}
fieldNameToColumnName :: Text -> Text
fieldNameToColumnName columnName = unwrapEither columnName $ Inflector.toUnderscore columnName

{-# INLINE pluralToSingular #-}
pluralToSingular :: Text -> Text
pluralToSingular w    | toLower w == "status"
                      || toLower w == "inprogress"
                      || toLower w == "in_progress"
                      = w
pluralToSingular word = fromMaybe word (stripSuffix "s" word)

{-# INLINE humanize #-}
humanize :: Text -> Text
humanize text = unwrapEither text $ Inflector.toHumanized True text

{-# INLINE applyFirst #-}
applyFirst :: (Text -> Text) -> Text -> Text
applyFirst f text =
    let (first, rest) = Data.Text.splitAt 1 text
    in (f first) <> rest

{-# INLINE lcfirst #-}
lcfirst :: Text -> Text
lcfirst = applyFirst Data.Text.toLower

{-# INLINE ucfirst #-}
ucfirst :: Text -> Text
ucfirst = applyFirst Data.Text.toUpper
