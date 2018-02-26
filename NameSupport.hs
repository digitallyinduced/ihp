module Foundation.NameSupport (tableNameToModelName, pluralToSingular) where

import ClassyPrelude
import Text.Inflections as Inflector
import Data.String.Conversions (cs)

-- `users` => `User`
-- `projects` => `Project`
tableNameToModelName :: Text -> Text
tableNameToModelName tableName =
    let (Right modelName) = Inflector.toCamelCased True $ cs (pluralToSingular tableName)
    in modelName

pluralToSingular :: Text -> Text
pluralToSingular word = fromMaybe word (stripSuffix "s" word)
