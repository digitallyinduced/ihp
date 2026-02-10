{-|
Module: Test.QueryBuilderSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.QueryBuilderSpec where

import Test.Hspec
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ModelSupport

data Post = Post
        { id :: UUID
        , title :: Text
        , externalUrl :: Maybe Text
        , createdAt :: UTCTime
        , public :: Bool
        , createdBy :: UUID
        , categoryId :: Maybe UUID
        }

type instance GetTableName Post = "posts"
type instance GetModelByTableName "posts" = Post
type instance PrimaryKey "posts" = UUID


data WeirdPkTag = WeirdPkTag
        { tagIden :: UUID
        , tagText :: Text
        }

type instance GetTableName WeirdPkTag = "weird_tags"
type instance GetModelByTableName "weird_tags" = WeirdPkTag 
type instance PrimaryKey "weird_tags" = UUID

instance Table WeirdPkTag where
    columnNames = ["tag_iden", "tag_text"]
    primaryKeyColumnNames = ["tag_iden"]


instance Table Post where
    columnNames = ["id", "title", "external_url", "created_at", "public", "created_by", "category_id"]
    primaryKeyColumnNames = ["id"]

data Tag = Tag
        { id :: UUID
        , tagText :: Text
        }

type instance GetTableName Tag = "tags"
type instance GetModelByTableName "tags" = Tag 
type instance PrimaryKey "tags" = UUID

instance Table Tag where
    columnNames = ["id", "tag_text"]
    primaryKeyColumnNames = ["id"]

data Tagging = Tagging 
        { id :: UUID
        , postId :: UUID
        , tagId :: UUID
        }


type instance GetTableName Tagging = "taggings"
type instance GetModelByTableName "taggings" = Tagging
type instance PrimaryKey "taggings" = UUID

instance Table Tagging where
    columnNames = ["id", "post_id", "tag_id"]
    primaryKeyColumnNames = ["id"]
    
data CompositeTagging = CompositeTagging 
        { postId :: UUID
        , tagId :: UUID
        }


type instance GetTableName CompositeTagging = "composite_taggings"
type instance GetModelByTableName "composite_taggings" = CompositeTagging
type instance PrimaryKey "composite_taggings" = (Id' "posts", Id' "tags")

instance Table CompositeTagging where
    columnNames = ["post_id", "tag_id"]
    primaryKeyColumnNames = ["post_id", "tag_id"]


data User = User
    { id :: UUID,
      name :: Text
    }

type instance GetTableName User = "users"
type instance GetModelByTableName "users" = User 
type instance PrimaryKey "users" = UUID

instance Table User where
    columnNames = ["id", "name"]
    primaryKeyColumnNames = ["id"]

data FavoriteTitle = FavoriteTitle
    {
        title :: Text,
        likes :: Int
    }

type instance GetTableName FavoriteTitle = "favorite_title"
type instance GetModelByTableName "favorite_title" = FavoriteTitle 

instance Table FavoriteTitle where
    columnNames = ["title", "likes"]
    primaryKeyColumnNames = []

tests = do
    describe "QueryBuilder" do
        let postColumns = "posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id"

        describe "query" do
            it "should provide a simple sql query" do
                let theQuery = query @Post

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts")

        describe "filterWhere" do
            it "should produce a SQL with a WHERE condition" do
                let theQuery = query @Post
                        |> filterWhere (#title, "Test" :: Text)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title = $1")

            it "should use IS operator for checking null" do
                let theQuery = query @Post
                        |> filterWhere (#externalUrl, Nothing)

                -- Note: hasql uses parameterized null ($1) rather than literal NULL
                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.external_url IS $1")

        describe "filterWhereNot" do
            it "should produce a SQL with a WHERE NOT condition" do
                let theQuery = query @Post
                        |> filterWhereNot (#title, "Test" :: Text)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title != $1")

            it "should use IS NOT operator for checking null" do
                let theQuery = query @Post
                        |> filterWhereNot (#externalUrl, Nothing)

                -- Note: hasql uses parameterized null ($1) rather than literal NULL
                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.external_url IS NOT $1")

        describe "filterWhereIn" do
            it "should use = ANY for IN clause" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereIn (#title, theValues)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title = ANY ($1)")

            it "should use = ANY for UUID IN clause" do
                let theValues :: [UUID] = ["b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"]
                let theQuery = query @Post
                        |> filterWhereIn (#id, theValues)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.id = ANY ($1)")

            describe "with Maybe / NULL values" do
                it "should handle [Just .., Nothing]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574", Nothing]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    -- Note: hasql uses parameterized null ($2) rather than literal NULL
                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE (posts.category_id = ANY ($1)) OR (posts.category_id IS $2)")

                it "should handle [Just ..]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574"]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.category_id = ANY ($1)")

                it "should handle [Nothing]" do
                    let theValues :: [Maybe UUID] = [Nothing]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    -- Note: hasql uses parameterized null ($1) rather than literal NULL
                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.category_id IS $1")

        describe "filterWhereNotIn" do
            it "should use <> ALL for NOT IN clause" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title <> ALL ($1)")

            it "should ignore an empty value list" do
                let theValues :: [Text] = []
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts")

            describe "with Maybe / NULL values" do
                it "should handle [Just .., Nothing]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574", Nothing]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    -- Note: hasql uses parameterized null ($2) rather than literal NULL
                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE (posts.category_id <> ALL ($1)) AND (posts.category_id IS NOT $2)")

                it "should handle [Just ..]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574"]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.category_id <> ALL ($1)")

                it "should handle [Nothing]" do
                    let theValues :: [Maybe UUID] = [Nothing]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    -- Note: hasql uses parameterized null ($1) rather than literal NULL
                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.category_id IS NOT $1")

        describe "filterWhereIdIn" do
            it "should use = ANY for Id IN clause" do
                let theValues :: [Id Post] = ["b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"]
                let theQuery = query @Post
                        |> filterWhereIdIn theValues

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.id = ANY ($1)")

            describe "with empty values" do
                it "should produce a SQL with a WHERE condition" do
                    let theValues :: [Id Post] = []
                    let theQuery = query @Post
                            |> filterWhereIdIn theValues

                    -- Empty list still uses the same pattern but with empty array
                    (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.id = ANY ($1)")

        describe "filterWhereCaseInsensitive" do
            it "should produce a SQL with a WHERE LOWER() condition" do
                let theQuery = query @Post
                        |> filterWhereCaseInsensitive (#title, "Test" :: Text)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE LOWER(posts.title) = LOWER($1)")

        describe "filterWhereILike" do
            it "should produce a SQL with a WHERE ILIKE condition" do
                let searchTerm = "good"
                let theQuery = query @Post
                        |> filterWhereILike (#title, "%" <> searchTerm <> "%")

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.title ILIKE $1")

        describe "filterWhereGreaterThan" do
            it "should produce a SQL with a WHERE > condition" do
                let theQuery = query @FavoriteTitle
                        |> filterWhereGreaterThan (#likes, 100 :: Int)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` "SELECT favorite_title.title, favorite_title.likes FROM favorite_title WHERE favorite_title.likes > $1"

        describe "filterWhereLessThan" do
            it "should produce a SQL with a WHERE < condition" do
                let theQuery = query @FavoriteTitle
                        |> filterWhereLessThan (#likes, 50 :: Int)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` "SELECT favorite_title.title, favorite_title.likes FROM favorite_title WHERE favorite_title.likes < $1"

        describe "filterWhereSql" do
            it "should produce a SQL with a raw WHERE condition" do
                let theQuery = query @Post
                        |> filterWhereSql (#createdAt, "< current_timestamp - interval '1 day'")

                -- Note: there's an extra space between column name and operator (consistent with postgresql-simple path)
                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE posts.created_at  < current_timestamp - interval '1 day'")

        describe "queryOr" do
            it "should merge two conditions" do
                let theQuery = query @Post
                        |> queryOr
                            (filterWhere (#createdBy, "fe41a985-36a3-4f14-b13c-c166977dc7e8" :: UUID))
                            (filterWhere (#public, True))

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE (posts.created_by = $1) OR (posts.public = $2)")

        describe "innerJoin" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title")

        describe "orderBy" do
            it "should add a ORDER BY" do
                let theQuery = query @Post
                        |> orderByAsc #createdAt

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts ORDER BY posts.created_at")

            it "should add a ORDER BY DESC" do
                let theQuery = query @Post
                        |> orderByDesc #createdAt

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts ORDER BY posts.created_at DESC")

        describe "limit" do
            it "should add a LIMIT" do
                let theQuery = query @Post
                        |> limit 1337

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts LIMIT 1337")

        describe "offset" do
            it "should add an OFFSET" do
                let theQuery = query @Post
                        |> offset 1337

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts OFFSET 1337")

        describe "distinct" do
            it "should add a DISTINCT" do
                let theQuery = query @Post
                        |> distinct

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT DISTINCT " <> postColumns <> " FROM posts")

        describe "distinctOn" do
            it "should add a DISTINCT ON (..)" do
                let theQuery = query @Post
                        |> distinctOn #title

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT DISTINCT ON (posts.title) " <> postColumns <> " FROM posts")

        describe "Complex Queries" do
            it "should allow a query with limit and offset" do
                let theQuery = query @Post
                        |> offset 20
                        |> limit 50

                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts LIMIT 50 OFFSET 20")

            it "should work with multiple complex conditions" do
                let theQuery = query @Post
                        |> queryOr
                            (\qb -> qb
                                |> filterWhere (#title, "test")
                                |> filterWhere (#public, True)
                                |> filterWhere (#externalUrl, Nothing)
                            )
                            (filterWhere (#createdBy, "e70c66fb-68a5-41b8-8bf1-85b9bb046d15" :: UUID))
                        |> orderBy #createdAt
                        |> orderBy #title
                        |> limit 10

                -- Note: hasql uses parameterized null ($3) rather than literal NULL
                (snippetToSQL $ toSnippet theQuery) `shouldBe` ("SELECT " <> postColumns <> " FROM posts WHERE (((posts.title = $1) AND (posts.public = $2)) AND (posts.external_url IS $3)) OR (posts.created_by = $4) ORDER BY posts.created_at,posts.title LIMIT 10")
