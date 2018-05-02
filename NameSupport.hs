module Foundation.NameSupport (tableNameToModelName, columnNameToFieldName, pluralToSingular, humanize, ucfirst, lcfirst) where

import           ClassyPrelude
import           Data.String.Conversions (cs)
import qualified Text.Inflections        as Inflector
import qualified Data.Text

-- `users` => `User`
-- `projects` => `Project`
tableNameToModelName :: Text -> Text
tableNameToModelName tableName =
    let (Right modelName) = Inflector.toCamelCased True $ cs (pluralToSingular tableName)
    in modelName

-- `email` => `email`
-- `project_id` => `projectId`
columnNameToFieldName :: Text -> Text
columnNameToFieldName columnName =
    let (Right fieldName) = Inflector.toCamelCased False columnName
    in fieldName

pluralToSingular :: Text -> Text
pluralToSingular w    | toLower w == "status"
                      || toLower w == "inprogress"
                      || toLower w == "in_progress"
                      = w
pluralToSingular word = fromMaybe word (stripSuffix "s" word)

humanize text = let (Right value) = Inflector.toHumanized True text in value

applyFirst :: (Text -> Text) -> Text -> Text
applyFirst f text =
    let (first, rest) = Data.Text.splitAt 1 text
    in (f first) <> rest

lcfirst :: Text -> Text
lcfirst = applyFirst Data.Text.toLower

ucfirst :: Text -> Text
ucfirst = applyFirst Data.Text.toUpper
