module Test.HasqlEncoderSpec where

import Test.Hspec
import IHP.Prelude
import IHP.Hasql.Encoders (ToSnippetParams(..), sqlToSnippet)
import IHP.ModelSupport.Types (Id'(..), PrimaryKey)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import Hasql.Implicits.Encoders ()
import Database.PostgreSQL.Simple (Only(..), (:.)(..))

-- Test type family instances for non-UUID primary keys
type instance PrimaryKey "countries" = Text
type instance PrimaryKey "serial_table" = Int
type instance PrimaryKey "users" = UUID

-- | Convert a Snippet to its SQL text representation for testing.
-- Parameters become $1, $2, etc.
snippetToSql :: Snippet -> Text
snippetToSql s = Statement.toSql (Snippet.toStatement s Decoders.noResult)

tests :: Spec
tests = do
    describe "Hasql Encoders" do
        describe "sqlToSnippet" do
            it "should handle SQL with no placeholders" do
                snippetToSql (sqlToSnippet "SELECT 1" [])
                    `shouldBe` "SELECT 1"

            it "should convert a single ? placeholder to $1" do
                snippetToSql (sqlToSnippet "SELECT * FROM users WHERE id = ?" [Snippet.param (42 :: Int64)])
                    `shouldBe` "SELECT * FROM users WHERE id = $1"

            it "should convert multiple ? placeholders to $1, $2" do
                snippetToSql (sqlToSnippet "WHERE id = ? AND name = ?" [Snippet.param (1 :: Int64), Snippet.param ("test" :: Text)])
                    `shouldBe` "WHERE id = $1 AND name = $2"

            it "should handle placeholders inside parentheses" do
                snippetToSql (sqlToSnippet "INSERT INTO t VALUES (?)" [Snippet.param (1 :: Int64)])
                    `shouldBe` "INSERT INTO t VALUES ($1)"

            it "should handle adjacent placeholders" do
                snippetToSql (sqlToSnippet "VALUES (?, ?)" [Snippet.param (1 :: Int64), Snippet.param (2 :: Int64)])
                    `shouldBe` "VALUES ($1, $2)"

        describe "ToSnippetParams" do
            it "should produce 0 params for ()" do
                length (toSnippetParams ()) `shouldBe` 0

            it "should produce 1 param for Only" do
                length (toSnippetParams (Only (42 :: Int64))) `shouldBe` 1

            it "should produce 2 params for a 2-tuple" do
                length (toSnippetParams ("a" :: Text, "b" :: Text)) `shouldBe` 2

            it "should produce 3 params for a 3-tuple" do
                length (toSnippetParams ("a" :: Text, "b" :: Text, "c" :: Text)) `shouldBe` 3

            it "should concatenate params with :." do
                length (toSnippetParams (Only (1 :: Int64) :. Only (2 :: Int64))) `shouldBe` 2

            it "should concatenate tuple + Only with :." do
                length (toSnippetParams ((1 :: Int64, 2 :: Int64) :. Only (3 :: Int64))) `shouldBe` 3

        describe "sqlToSnippet + toSnippetParams end-to-end" do
            it "should parameterize a single-param query" do
                snippetToSql (sqlToSnippet "SELECT ?" (toSnippetParams (Only (42 :: Int64))))
                    `shouldBe` "SELECT $1"

            it "should parameterize a two-param query" do
                snippetToSql (sqlToSnippet "SELECT ?, ?" (toSnippetParams ("a" :: Text, "b" :: Text)))
                    `shouldBe` "SELECT $1, $2"

            it "should handle no-param query" do
                snippetToSql (sqlToSnippet "SELECT 1" (toSnippetParams ()))
                    `shouldBe` "SELECT 1"

            it "should parameterize a three-param query" do
                snippetToSql (sqlToSnippet "WHERE a = ? AND b = ? AND c = ?" (toSnippetParams (1 :: Int64, 2 :: Int64, 3 :: Int64)))
                    `shouldBe` "WHERE a = $1 AND b = $2 AND c = $3"

            it "should parameterize with :. concatenation" do
                snippetToSql (sqlToSnippet "SELECT * FROM t LIMIT ? OFFSET ?" (toSnippetParams (Only (50 :: Int64) :. Only (100 :: Int64))))
                    `shouldBe` "SELECT * FROM t LIMIT $1 OFFSET $2"

        describe "Id' with non-UUID primary keys" do
            it "should encode Text-keyed Id' as a snippet param" do
                let countryId :: Id' "countries" = Id "US"
                snippetToSql (sqlToSnippet "SELECT * FROM countries WHERE id = ?" [Snippet.param countryId])
                    `shouldBe` "SELECT * FROM countries WHERE id = $1"

            it "should encode Int-keyed Id' as a snippet param" do
                let serialId :: Id' "serial_table" = Id 42
                snippetToSql (sqlToSnippet "SELECT * FROM serial_table WHERE id = ?" [Snippet.param serialId])
                    `shouldBe` "SELECT * FROM serial_table WHERE id = $1"

            it "should encode UUID Id' as a snippet param (regression)" do
                let userId :: Id' "users" = Id "550e8400-e29b-41d4-a716-446655440000"
                snippetToSql (sqlToSnippet "SELECT * FROM users WHERE id = ?" [Snippet.param userId])
                    `shouldBe` "SELECT * FROM users WHERE id = $1"

            it "should encode Maybe (Id' with Text key) as a snippet param" do
                let countryId :: Maybe (Id' "countries") = Just (Id "US")
                snippetToSql (sqlToSnippet "SELECT * FROM t WHERE country_id = ?" [Snippet.param countryId])
                    `shouldBe` "SELECT * FROM t WHERE country_id = $1"

            it "should encode [Id' with Text key] as a snippet param" do
                let countryIds :: [Id' "countries"] = [Id "US", Id "DE"]
                snippetToSql (sqlToSnippet "SELECT * FROM countries WHERE id = ANY(?)" [Snippet.param countryIds])
                    `shouldBe` "SELECT * FROM countries WHERE id = ANY($1)"
