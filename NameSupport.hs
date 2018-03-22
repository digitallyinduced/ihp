module Foundation.NameSupport (tableNameToModelName, pluralToSingular, humanize) where

import           ClassyPrelude
import           Data.String.Conversions (cs)
import qualified Text.Inflections        as Inflector

-- `users` => `User`
-- `projects` => `Project`
tableNameToModelName :: Text -> Text
tableNameToModelName tableName =
    let (Right modelName) = Inflector.toCamelCased True $ cs (pluralToSingular tableName)
    in modelName

pluralToSingular :: Text -> Text
pluralToSingular w    | toLower w == "status"
                      || toLower w == "inprogress"
                      || toLower w == "in_progress"
                      = w
pluralToSingular word = fromMaybe word (stripSuffix "s" word)

humanize text = let (Right value) = Inflector.toHumanized True text in value
