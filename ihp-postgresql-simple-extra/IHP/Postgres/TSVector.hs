{-# LANGUAGE TemplateHaskell #-}
{-|
Module: IHP.Postgres.TSVector
Description: Adds support for the Postgres tsvector type
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.Postgres.TSVector
( TSVector
, module PostgresqlTypes.Tsvector
) where

import BasicPrelude
import IHP.Postgres.TypeInfo
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.TypeInfo.Macro
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Attoparsec.ByteString.Char8 as Attoparsec hiding (Parser(..))
import Data.Attoparsec.Internal.Types (Parser)
import Data.ByteString.Builder (byteString)
import PostgresqlTypes.Tsvector

-- | Type alias for backwards compatibility
type TSVector = Tsvector

instance FromField Tsvector where
    fromField f v =
        if typeOid f /= $(inlineTypoid tsvector)
        then returnError Incompatible f ""
        else case v of
               Nothing -> returnError UnexpectedNull f ""
               Just bs ->
                   case parseOnly parseTsvectorBS bs of
                     Left  err -> returnError ConversionFailed f err
                     Right val -> pure val

-- | Parse tsvector text representation from ByteString
-- Format: 'word1':1A,2B 'word2':3C
parseTsvectorBS :: Parser ByteString Tsvector
parseTsvectorBS = do
    lexemes <- parseLexeme `sepBy` skipSpace1
    case fromLexemeList lexemes of
        Just tv -> pure tv
        Nothing -> fail "invalid tsvector lexeme"
    where
        skipSpace1 = skipWhile (== ' ')
        parseLexeme = do
            skipSpace
            char '\''
            token <- Attoparsec.takeWhile (/= '\'')
            char '\''
            char ':'
            positions <- parsePosition `sepBy1` char ','
            pure (TextEncoding.decodeUtf8 token, positions)
        parsePosition = do
            pos <- Attoparsec.decimal
            weight <- option WeightD $ choice
                [ char 'A' >> pure WeightA
                , char 'B' >> pure WeightB
                , char 'C' >> pure WeightC
                , char 'D' >> pure WeightD
                ]
            pure (pos, weight)

instance ToField Tsvector where
    toField tv = Many $ intersperse (Plain " ") $ map serializeLexeme (toLexemeList tv)
        where
            serializeLexeme (token, positions) = Many
                [ Plain $ byteString "'"
                , Plain $ byteString $ TextEncoding.encodeUtf8 token
                , Plain $ byteString "'"
                , Plain $ byteString ":"
                , Many $ intersperse (Plain $ byteString ",") (map serializePosition positions)
                ]
            serializePosition (pos, weight) = Many
                [ toField (fromIntegral pos :: Int)
                , Plain $ byteString $ case weight of
                    WeightA -> "A"
                    WeightB -> "B"
                    WeightC -> "C"
                    WeightD -> ""
                ]
