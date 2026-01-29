{-|
Module: IHP.Postgres.TSVector
Description: Adds support for the Postgres tsvector type
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TSVector where

import BasicPrelude
import Data.Attoparsec.ByteString.Char8 as Attoparsec hiding (Parser(..))
import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Represents a Postgres tsvector
--
-- See https://www.postgresql.org/docs/current/datatype-textsearch.html
data TSVector
    = TSVector [Lexeme]
    deriving (Eq, Show, Ord)

data Lexeme
    = Lexeme { token :: Text, ranking :: [LexemeRanking] }
    deriving (Eq, Show, Ord)

data LexemeRanking
    = LexemeRanking { position :: Int, weight :: Char }
    deriving (Eq, Show, Ord)

-- 'a:1A fat:2B,4C cat:5D'
-- 'descript':4 'one':1,3 'titl':2
parseTSVector :: Parser ByteString TSVector
parseTSVector = TSVector <$> many' parseLexeme
    where
        parseLexeme = do
            skipSpace

            char '\''
            token <- Attoparsec.takeWhile (/= '\'')
            char '\''

            char ':'
            ranking <- many1 do
                skipMany $ char ','

                position <- double
                -- The Default Weight Is `D` So Postgres Does Not Include It In The Result
                weight <- option 'D' $ choice [char 'A', char 'B', char 'C', char 'D']
                pure $ LexemeRanking { position = truncate position, weight }

            pure $ Lexeme { token = Text.decodeUtf8 token, ranking }

-- | Serialize a TSVector to its Postgres text representation
tsvectorToText :: TSVector -> Text
tsvectorToText (TSVector lexemes) = Text.unwords (map lexemeToText lexemes)
    where
        lexemeToText :: Lexeme -> Text
        lexemeToText Lexeme { token, ranking } =
            "'" <> token <> "':" <> Text.intercalate "," (map rankingToText ranking)

        rankingToText :: LexemeRanking -> Text
        rankingToText LexemeRanking { position, weight } =
            tshow position <> (if weight == 'D' then "" else Text.singleton weight)
