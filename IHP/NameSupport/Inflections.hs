module IHP.NameSupport.Inflections
( pluralize
, singularize
) where

-- import IHP.Prelude
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
inflect :: Text -> Int -> Text
inflect t i = case i of
    1 -> singularize t
    _ -> pluralize t

-- | inflect a word given any number and inflection mapping
inflectWith :: [Inflection] -> Text -> Int -> Text
inflectWith l t i = case i of
    1 -> singularizeWith l t
    _ -> pluralizeWith l t

lookupWith :: (Text -> Inflection -> Maybe Text) -> [Inflection] -> Text -> Text
lookupWith f mapping target = fromMaybe target $ listToMaybe matches
    where
        matches = mapMaybe (f target) (reverse mapping)

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeMatchMapping :: [(RegexPattern, RegexReplace)] -> [Inflection]
makeMatchMapping = fmap (\(pat, rep) -> Match (regexPattern pat, rep))

-- | Makes a simple list of mappings from singular to plural, e.g [("person", "people")]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeIrregularMapping :: [(Singular, Plural)] -> [Inflection]
makeIrregularMapping = fmap Simple

-- | Makes a simple list of uncountables which don't have
-- singular plural versions, e.g ["fish", "money"]
-- the output of [Inflection] should be consumed by `singularizeWith` or `pluralizeWith`
makeUncountableMapping :: [Text] -> [Inflection]
makeUncountableMapping = fmap (\a -> Simple (a, a))

defaultPlurals :: [Inflection]
defaultPlurals = makeMatchMapping defaultPlurals'

defaultSingulars :: [Inflection]
defaultSingulars = makeMatchMapping defaultSingulars'

defaultIrregulars :: [Inflection]
defaultIrregulars = makeIrregularMapping defaultIrregulars'

defaultUncountables :: [Inflection]
defaultUncountables = makeUncountableMapping defaultUncountables'

pluralLookup :: Text -> Inflection -> Maybe Text
pluralLookup t (Match (r1, r2)) = runSub (r1, r2) t
pluralLookup t (Simple (a, b)) = if t == a then Just b else Nothing

singularLookup :: Text -> Inflection -> Maybe Text
singularLookup t (Match (r1, r2)) = runSub (r1, r2) t
singularLookup t (Simple (a, b)) = if t == b then Just a else Nothing

runSub :: (Maybe Regex, RegexReplace) -> Text -> Maybe Text
runSub (Nothing, _) _ = Nothing
runSub (Just reg, rep) t = matchWithReplace (reg, rep) t

matchWithReplace :: (Regex, RegexReplace) -> Text -> Maybe Text
matchWithReplace (reg, rep) t =
    if regexMatch t reg
        then toMaybe $ substitute' reg (encodeUtf8 t) (encodeUtf8 rep)
        else Nothing
    where
        toMaybe = either (const Nothing) (Just . decodeUtf8)

regexMatch :: Text -> Regex -> Bool
regexMatch t r = case match of
    Left _ -> False
    Right m -> isJust m
    where
        match = unsafePerformIO $ execute r (encodeUtf8 t)

regexPattern :: Text -> Maybe Regex
regexPattern pat = toMaybe reg
    where
        toMaybe = either (const Nothing) Just
        reg = unsafePerformIO $ compile compCaseless execBlank (encodeUtf8 pat)
