{-|
Module: Test.FetchPipelinedSpec
Description: Tests for IHP.Fetch.Statement builders and IHP.FetchPipelined
Copyright: (c) digitally induced GmbH, 2026
-}
module Test.FetchPipelinedSpec where

import Test.Hspec
import IHP.Prelude
import IHP.QueryBuilder
import IHP.Fetch.Statement (buildQueryListStatement, buildQueryMaybeStatement, buildCountStatement, buildExistsStatement)
import qualified Hasql.Statement as Hasql
import Test.ModelFixtures

-- Helper to extract SQL from a Hasql Statement
stmtSql :: Hasql.Statement a b -> Text
stmtSql = Hasql.toSql

tests :: Spec
tests = do
    let postColumns = "posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id"
    let userColumns = "users.id, users.name"

    describe "IHP.Fetch.Statement" do
        describe "buildQueryListStatement" do
            it "should produce SELECT for all rows" do
                let stmt = buildQueryListStatement (query @Post)
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts")

            it "should include WHERE for filterWhere" do
                let stmt = buildQueryListStatement (query @Post |> filterWhere (#title, "Test" :: Text))
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title = $1")

            it "should include ORDER BY" do
                let stmt = buildQueryListStatement (query @Post |> orderByDesc #createdAt)
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts ORDER BY posts.created_at DESC")

            it "should include LIMIT as parameter" do
                let stmt = buildQueryListStatement (query @Post |> limit 10)
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts LIMIT $1")

            it "should work with a different model" do
                let stmt = buildQueryListStatement (query @User)
                stmtSql stmt `shouldBe` ("SELECT " <> userColumns <> " FROM users")

        describe "buildQueryMaybeStatement" do
            it "should add LIMIT for single-row fetch" do
                let stmt = buildQueryMaybeStatement (query @Post)
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts LIMIT $1")

            it "should include WHERE and LIMIT" do
                let stmt = buildQueryMaybeStatement (query @Post |> filterWhere (#title, "Test" :: Text))
                stmtSql stmt `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title = $1 LIMIT $2")

        describe "buildCountStatement" do
            it "should wrap query in COUNT(*)" do
                let stmt = buildCountStatement (query @Post)
                stmtSql stmt `shouldBe` ("SELECT COUNT(*) FROM (SELECT " <> postColumns <> " FROM posts) AS _count_values")

            it "should preserve WHERE in inner query" do
                let stmt = buildCountStatement (query @Post |> filterWhere (#public, True))
                stmtSql stmt `shouldBe` ("SELECT COUNT(*) FROM (SELECT " <> postColumns <> " FROM posts WHERE posts.public = $1) AS _count_values")

        describe "buildExistsStatement" do
            it "should wrap query in EXISTS" do
                let stmt = buildExistsStatement (query @Post)
                stmtSql stmt `shouldBe` ("SELECT EXISTS (SELECT " <> postColumns <> " FROM posts) AS _exists_values")

            it "should preserve WHERE in inner query" do
                let stmt = buildExistsStatement (query @Post |> filterWhere (#public, True))
                stmtSql stmt `shouldBe` ("SELECT EXISTS (SELECT " <> postColumns <> " FROM posts WHERE posts.public = $1) AS _exists_values")
