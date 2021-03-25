module IHP.NameSupport.Inflections
( pluralize
, singularize
, inflect
) where

import Prelude
import Data.Maybe (mapMaybe, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)
import IHP.NameSupport.Inflections.Data
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils (substitute')

type RegexPattern = Text
type RegexReplace = Text
type Singular = Text
type Plural = Text

data Inflection
    = Simple (Singular, Plural)
    | Match (Maybe Regex, RegexReplace)

-- | pluralize a given word
-- >>> pluralize "person"
-- "people"
-- >>> pluralize "dog"
-- "dogs"
pluralize :: Text -> Text
pluralize = pluralizeWith defaultPluralizeMapping

-- | default mappings for pluralization
defaultPluralizeMapping :: [Inflection]
defaultPluralizeMapping = defaultPlurals ++ defaultUncountables ++ defaultIrregulars

-- | singularize a given word
-- >>> singularize "people"
-- "person"
-- >>> singularize "cats"
-- "cat"
singularize :: Text -> Text
singularize = singularizeWith defaultSingularizeMapping

-- | default mappings for singularization
defaultSingularizeMapping :: [Inflection]
defaultSingularizeMapping = defaultSingulars ++ defaultUncountables ++ defaultIrregulars

-- | pluralize a word given a custom mapping.
-- Build the [Inflection] with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`
pluralizeWith :: [Inflection] -> Text -> Text
pluralizeWith = lookupWith pluralLookup

-- | singularize a word given a custom mapping.
-- Build the [Inflection] with a combination of
-- `makeUncountableMapping` `makeIrregularMapping` `makeMatchMapping`
singularizeWith :: [Inflection] -> Text -> Text
singularizeWith = lookupWith singularLookup

-- | inflect a word given any number
-- >>> inflect "person" 1
-- "person"
-- >>> inflect "person" 2
-- "people"
inflect :: Text -> Int -> Text
inflect word count = case count of
    1 -> singularize word
    _ -> pluralize word

-- | inflect a word given any number and inflection mapping
inflectWith :: [Inflection] -> Text -> Int -> Text
inflectWith inflections text count = case count of
    1 -> singularizeWith inflections text
    _ -> pluralizeWith inflections text

lookupWith :: (Text -> Inflection -> Maybe Text) -> [Inflection] -> Text -> Text
lookupWith f mapping target = fromMaybe target $ listToMaybe matches
    where
        matches = mapMaybe (f target) (reverse mapping)

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeMatchMapping :: [(RegexPattern, RegexReplace)] -> [Inflection]
makeMatchMapping = fmap (\(pattern, replacement) -> Match (regexPattern pattern, replacement))

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeIrregularMapping :: [(Singular, Plural)] -> [Inflection]
makeIrregularMapping = fmap Simple

-- | Makes a simple list of uncountables which don't have
-- singular plural versions, e.g ["fish", "money"]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeUncountableMapping :: [Text] -> [Inflection]
makeUncountableMapping = fmap (\text -> Simple (text, text))

defaultPlurals :: [Inflection]
defaultPlurals = makeMatchMapping defaultPlurals'

defaultSingulars :: [Inflection]
defaultSingulars = makeMatchMapping defaultSingulars'

defaultIrregulars :: [Inflection]
defaultIrregulars = makeIrregularMapping defaultIrregulars'

defaultUncountables :: [Inflection]
defaultUncountables = makeUncountableMapping defaultUncountables'

pluralLookup :: Text -> Inflection -> Maybe Text
pluralLookup word (Match (pattern, replacement)) = runSubstitution (pattern, replacement) word
pluralLookup word (Simple (singular, plural)) = if word == singular then Just plural else Nothing

singularLookup :: Text -> Inflection -> Maybe Text
singularLookup word (Match (regex, replacement)) = runSubstitution (regex, replacement) word
singularLookup word (Simple (singular, plural)) = if word == plural then Just singular else Nothing

runSubstitution :: (Maybe Regex, RegexReplace) -> Text -> Maybe Text
runSubstitution (Nothing, _) _ = Nothing
runSubstitution (Just regex, replacement) text = matchWithReplace (regex, replacement) text

matchWithReplace :: (Regex, RegexReplace) -> Text -> Maybe Text
matchWithReplace (regex, replacement) text =
    if regexMatch text regex
        then toMaybe $ substitute' regex (encodeUtf8 text) (encodeUtf8 replacement)
        else Nothing
    where
        toMaybe = either (const Nothing) (Just . decodeUtf8)

regexMatch :: Text -> Regex -> Bool
regexMatch text regex = case match of
    Left _ -> False
    Right m -> isJust m
    where
        match = unsafePerformIO $ execute regex (encodeUtf8 text)

regexPattern :: Text -> Maybe Regex
regexPattern pattern = toMaybe regex
    where
        toMaybe = either (const Nothing) Just
        regex = unsafePerformIO $ compile compCaseless execBlank $ encodeUtf8 pattern
