{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.TSVector
Description: Adds support for the Postgres tsvector type
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TSVector where

import BasicPrelude
import Data.String.Conversions (cs)
import IHP.Postgres.TypeInfo
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.TypeInfo.Macro
import Data.Attoparsec.ByteString.Char8 as Attoparsec

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

instance FromField TSVector where
    fromField f v =
        if typeOid f /= $(inlineTypoid tsvector)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parser bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val
      where
        -- 'a:1A fat:2B,4C cat:5D'
        -- 'descript':4 'one':1,3 'titl':2
        parser = TSVector <$> many' parseLexeme
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

            pure $ Lexeme { token = cs token, ranking }

instance ToField TSVector where
    toField = undefined
