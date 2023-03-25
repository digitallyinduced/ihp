module IHP.NameSupport.Inflections.Data where

import Data.Text (Text)

-- | These default inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultPlurals' :: [(Text, Text)]
defaultPlurals' =
    [ ("$", "s")
    , ("s$", "s")
    , ("^(ax|test)is$", "\\1es")
    , ("(octop|vir)us$", "\\1i")
    , ("(octop|vir)i$", "\\1i")
    , ("(alias|status)$", "\\1es")
    , ("(bu)s$", "\\1ses")
    , ("(buffal|tomat)o$", "\\1oes")
    , ("([ti])um$", "\\1a")
    , ("([ti])a$", "\\1a")
    , ("sis$", "ses")
    , ("(?:([^f])fe|([lr])f)$", "\\1\2ves")
    , ("(hive)$", "\\1s")
    , ("([^aeiouy]|qu)y$", "\\1ies")
    , ("(x|ch|ss|sh)$", "\\1es")
    , ("(matr|vert|ind)(?:ix|ex)$", "\\1ices")
    , ("^(m|l)ouse$", "\\1ice")
    , ("^(m|l)ice$", "\\1ice")
    , ("^(ox)$", "\\1en")
    , ("^(oxen)$", "\\1")
    , ("(quiz)$", "\\1zes")
    ]

-- | These default inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultSingulars' :: [(Text, Text)]
defaultSingulars' =
    [ ("s$", "")
    , ("(ss)$", "\\1")
    , ("(n)ews$", "\\1ews")
    , ("([ti])a$", "\\1um")
    , ("((a)naly|(b)a|(d)iagno|(p)arenthe|(p)rogno|(s)ynop|(t)he)(sis|ses)$", "\\1sis")
    , ("(^analy)(sis|ses)$", "\\1sis")
    , ("([^f])ves$", "\\1fe")
    , ("(hive)s$", "\\1")
    , ("(tive)s$", "\\1")
    , ("([lr])ves$", "\\1f")
    , ("([^aeiouy]|qu)ies$", "\\1y")
    , ("(s)eries$", "\\1eries")
    , ("(m)ovies$", "\\1ovie")
    , ("(x|ch|ss|sh)es$", "\\1")
    , ("^(m|l)ice$", "\\1ouse")
    , ("(bus)(es)?$", "\\1")
    , ("(o)es$", "\\1")
    , ("(shoe)s$", "\\1")
    , ("(cris|test)(is|es)$", "\\1is")
    , ("^(a)x[ie]s$", "\\1xis")
    , ("(octop|vir)(us|i)$", "\\1us")
    , ("(alias|status)(es)?$", "\\1")
    , ("^(ox)en", "\\1")
    , ("(vert|ind)ices$", "\\1ex")
    , ("(matr)ices$", "\\1ix")
    , ("(quiz)zes$", "\\1")
    , ("(database)s$", "\\1")
    ]

-- | These default irregular inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultIrregulars' :: [(Text, Text)]
defaultIrregulars' =
    -- from singular to plural
    [ ("person", "people")
    , ("man", "men")
    , ("child", "children")
    , ("sex", "sexes")
    , ("move", "moves")
    , ("zombie", "zombies")
    ]

-- | These default uncountable inflections stolen from the Ruby inflection library - see
-- https://github.com/rails/rails/blob/master/activesupport/lib/active_support/inflections.rb
defaultUncountables' :: [Text]
defaultUncountables' =
    [ "equipment"
    , "information"
    , "rice"
    , "money"
    , "species"
    , "series"
    , "fish"
    , "sheep"
    , "jeans"
    , "police"
    ]
