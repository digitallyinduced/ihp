{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module: Test.TypedSqlSpec
-}
module Test.TypedSqlSpec where

import qualified Data.ByteString.Char8            as ByteString
import           Database.PostgreSQL.Simple.Types (Query (..))
import           IHP.ModelSupport
import           IHP.Prelude
import           IHP.TypedSql
import qualified Language.Haskell.TH.Syntax       as TH
import           System.Environment               (setEnv)
import           Test.Hspec

$(do
    TH.runIO (setEnv "IHP_TYPED_SQL_STUB" "Test/Test/TypedSqlStub.json")
    pure []
 )

-- Define primary keys for the tables referenced in the stub metadata

type instance PrimaryKey "users" = UUID
type instance PrimaryKey "posts" = UUID

tests :: Spec
tests = describe "TypedSql" do
    it "infers Id and Text columns" do
        let userId :: UUID
            userId = "11111111-1111-1111-1111-111111111111"
            typed :: TypedQuery (Id' "users", Text)
            typed = [typedSql|SELECT users.id, users.name FROM users WHERE users.id = ${userId}|]
            Query sqlBytes = tqQuery typed
        sqlBytes `shouldBe` ByteString.pack "SELECT users.id, users.name FROM users WHERE users.id = $1"

    it "maps nullable foreign keys to Maybe Id'" do
        let slug :: Text
            slug = "hello-world"
            typed :: TypedQuery (Maybe (Id' "users"))
            typed = [typedSql|SELECT posts.author_id FROM posts WHERE posts.slug = ${slug}|]
        length (tqParams typed) `shouldBe` 1

    it "infers aggregate columns" do
        let typed :: TypedQuery (Maybe Integer)
            typed = [typedSql|SELECT COUNT(*) FROM posts|]
            Query sqlBytes = tqQuery typed
        sqlBytes `shouldBe` ByteString.pack "SELECT COUNT(*) FROM posts"
