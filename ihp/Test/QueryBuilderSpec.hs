{-|
Module: Test.QueryBuilderSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.QueryBuilderSpec where

import Test.Hspec
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple.ToField as ToField
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (toField))
import qualified Data.ByteString.Builder as ByteString

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
    primaryKeyConditionForId (Id id) = toField id


instance Table Post where
    columnNames = ["id", "title", "external_url", "created_at", "public", "created_by", "category_id"]
    primaryKeyColumnNames = ["id"]
    primaryKeyConditionForId (Id id) = toField id

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
    primaryKeyConditionForId (Id id) = toField id

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
    primaryKeyConditionForId (Id id) = toField id
    
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
    primaryKeyConditionForId (Id (postId, tagId)) = Many ([Plain "(", toField postId, Plain ",", toField tagId, Plain ")"])


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
    primaryKeyConditionForId (Id id) = toField id

data FavoriteTitle = FavoriteTitle
    {
        title :: Text,
        likes :: Int
    }

type instance GetTableName FavoriteTitle = "favorite_title"
type instance GetModelByTableName "favorite_title" = FavoriteTitle 

instance Table FavoriteTitle where
    columnNames = ["title", "likes"]
    primaryKeyConditionForId _ = Many []
    primaryKeyColumnNames = []

tests = do
    describe "QueryBuilder" do
        describe "query" do
            it "should provide a simple sql query" do
                let theQuery = query @Post

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts", [])

        describe "filterWhere" do
            it "should produce a SQL with a WHERE condition" do
                let theQuery = query @Post
                        |> filterWhere (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.title = ?", [Escape "Test"])

            it "should use a IS operator for checking null" do
                let theQuery = query @Post
                        |> filterWhere (#externalUrl, Nothing)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.external_url IS ?", [Plain "null"])

        describe "filterWhereNot" do
            it "should produce a SQL with a WHERE NOT condition" do
                let theQuery = query @Post
                        |> filterWhereNot (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.title != ?", [Escape "Test"])

            it "should use a IS NOT operator for checking null" do
                let theQuery = query @Post
                        |> filterWhereNot (#externalUrl, Nothing)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.external_url IS NOT ?", [Plain "null"])

        describe "filterWhereIn" do
            it "should work with #id if the Model is suitable" do
                let theValues :: [UUID] = ["b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"]
                let theQuery = query @Post
                        |> filterWhereIn (#id, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.id IN ?", [Many [Plain "(", Plain "'b80e37a8-41d4-4731-b050-a716879ef1d1'", Plain ",", Plain "'629b7ee0-3675-4b02-ba3e-cdbd7b513553'", Plain ")"]])
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.title IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

            describe "with Maybe / NULL values" do
                it "should handle [Just .., Nothing]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574", Nothing]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE (posts.category_id IN ?) OR (posts.category_id IS ?)", [Many [Plain "(", Plain "'44dcf2cf-a79d-4caf-a2ea-427838ba3574'", Plain ")"], Plain "null"])

                it "should handle [Just ..]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574"]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.category_id IN ?", [Many [Plain "(", Plain "'44dcf2cf-a79d-4caf-a2ea-427838ba3574'", Plain ")"]])

                it "should handle [Nothing]" do
                    let theValues :: [Maybe UUID] = [Nothing]
                    let theQuery = query @Post
                            |> filterWhereIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.category_id IS ?", [Plain "null"])

        describe "filterWhereInCaseInsensitive" do
            it "should produce a SQL with a WHERE LOWER() condition" do
                let theQuery = query @Post
                        |> filterWhereInCaseInsensitive (#title, ["Test" :: Text, "Test 1" :: Text])

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE LOWER(posts.title) IN ?", [Many [Plain "(", Escape "test", Plain ",", Escape "test 1", Plain ")"]])

        describe "filterWhereIdIn" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Id Post] = ["b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"]
                let theQuery = query @Post
                        |> filterWhereIdIn theValues

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.id IN ?", [Many [Plain "(", Plain "'b80e37a8-41d4-4731-b050-a716879ef1d1'", Plain ",", Plain "'629b7ee0-3675-4b02-ba3e-cdbd7b513553'", Plain ")"]])

            describe "with empty values" do
                it "should produce a SQL with a WHERE condition" do
                    let theValues :: [Id Post] = []
                    let theQuery = query @Post
                            |> filterWhereIdIn theValues

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.id IN ?", [Plain "(null)"])

            describe "with weird primary key name" do
                it "should produce a SQL with a WHERE condition" do
                    let theValues :: [Id WeirdPkTag] = ["b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"]
                    let theQuery = query @WeirdPkTag
                            |> filterWhereIdIn theValues

                    (toSQL theQuery) `shouldBe` ("SELECT weird_tags.tag_iden, weird_tags.tag_text FROM weird_tags WHERE weird_tags.tag_iden IN ?", [Many [Plain "(", Plain "'b80e37a8-41d4-4731-b050-a716879ef1d1'", Plain ",", Plain "'629b7ee0-3675-4b02-ba3e-cdbd7b513553'", Plain ")"]])
            describe "with composite keys" do
                it "should produce a SQL with a WHERE condition" do
                    let theValues :: [Id CompositeTagging] = [Id ("b80e37a8-41d4-4731-b050-a716879ef1d1", "629b7ee0-3675-4b02-ba3e-cdbd7b513553"), Id ("8e2ef0ef-f680-4fcf-837d-7e3171385621", "95096f81-8ca6-407f-a263-cbc33546a828")]
                    let theQuery = query @CompositeTagging
                            |> filterWhereIdIn theValues

                    (toSQL theQuery) `shouldBe` ("SELECT composite_taggings.post_id, composite_taggings.tag_id FROM composite_taggings WHERE (composite_taggings.post_id, composite_taggings.tag_id) IN ?", [Many [Plain "(", Many [ Plain "(", Plain "'b80e37a8-41d4-4731-b050-a716879ef1d1'", Plain ",", Plain "'629b7ee0-3675-4b02-ba3e-cdbd7b513553'", Plain ")" ], Plain ",", Many [ Plain "(", Plain "'8e2ef0ef-f680-4fcf-837d-7e3171385621'", Plain ",", Plain "'95096f81-8ca6-407f-a263-cbc33546a828'", Plain ")"], Plain ")"]])

                describe "with empty values" do
                    it "should produce a SQL with a WHERE condition" do
                        let theValues :: [Id CompositeTagging] = []
                        let theQuery = query @CompositeTagging
                                |> filterWhereIdIn theValues

                        (toSQL theQuery) `shouldBe` ("SELECT composite_taggings.post_id, composite_taggings.tag_id FROM composite_taggings WHERE (composite_taggings.post_id, composite_taggings.tag_id) IN ?", [Plain "(null)"])


        describe "filterWhereInJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.id, users.name FROM users INNER JOIN posts ON users.name = posts.title WHERE posts.title IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])


        describe "filterWhereNotIn" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.title NOT IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

            it "ignore an empty value list as this causes the query to always return nothing" do
                let theValues :: [Text] = []
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts", [])

            describe "with Maybe / NULL values" do
                it "should handle [Just .., Nothing]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574", Nothing]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE (posts.category_id NOT IN ?) AND (posts.category_id IS NOT ?)", [Many [Plain "(", Plain "'44dcf2cf-a79d-4caf-a2ea-427838ba3574'", Plain ")"], Plain "null"])

                it "should handle [Just ..]" do
                    let theValues :: [Maybe UUID] = ["44dcf2cf-a79d-4caf-a2ea-427838ba3574"]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.category_id NOT IN ?", [Many [Plain "(", Plain "'44dcf2cf-a79d-4caf-a2ea-427838ba3574'", Plain ")"]])

                it "should handle [Nothing]" do
                    let theValues :: [Maybe UUID] = [Nothing]
                    let theQuery = query @Post
                            |> filterWhereNotIn (#categoryId, theValues)

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.category_id IS NOT ?", [Plain "null"])

        describe "filterWhereNotInJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereNotInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.id, users.name FROM users INNER JOIN posts ON users.name = posts.title WHERE posts.title NOT IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

            it "ignore an empty value list as this causes the query to always return nothing" do
                let theValues :: [Text] = []
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereNotInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.id, users.name FROM users INNER JOIN posts ON users.name = posts.title", [])

        describe "filterWhereILike" do
            it "should produce a SQL with a WHERE condition" do
                let searchTerm = "good"
                let theQuery = query @Post
                     |> filterWhereILike (#title, "%" <> searchTerm <> "%")
                (toSQL theQuery `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.title ILIKE ?", [Escape "%good%"]))

        describe "filterWhereILikeJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let searchTerm = "louis"
                let theQuery = query @Post
                     |> innerJoin @User (#createdBy, #id)
                     |> filterWhereILikeJoinedTable @User (#name, "%" <> searchTerm <> "%")
                (toSQL theQuery `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name ILIKE ?", [Escape "%louis%"]))
        
        describe "filterWherePast" do
            it "should produce a SQL with the correct WHERE condition" do
                let theQuery = query @Post
                        |> filterWherePast #createdAt

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.created_at  ?", [Plain "<= NOW()"])
  
        describe "filterWhereFuture" do
            it "should produce a SQL with the correct WHERE condition" do
                let theQuery = query @Post
                        |> filterWhereFuture #createdAt

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.created_at  ?", [Plain "> NOW()"])

        describe "filterWhereSql" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereSql (#createdAt, "< current_timestamp - interval '1 day'")

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE posts.created_at  ?", [Plain "< current_timestamp - interval '1 day'"])

        describe "filterWhereCaseInsensitive" do
            it "should produce a SQL with a WHERE LOWER() condition" do
                let theQuery = query @Post
                        |> filterWhereCaseInsensitive (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE LOWER(posts.title) = LOWER(?)", [Escape "Test"])

        describe "innerJoin" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title", [])


        describe "innerJoinThirdTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> innerJoinThirdTable @User @FavoriteTitle (#name, #title)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title INNER JOIN users ON favorite_title.title = users.name", [])

        describe "filterWhereJoinedTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> filterWhereJoinedTable @User (#name, "Tom" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title WHERE users.name = ?", [Escape "Tom"])

        describe "filterWhereNotJoinedTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> filterWhereNotJoinedTable @User (#name, "Tom" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title WHERE users.name != ?", [Escape "Tom"])


        describe "labelResults" do
            it "should provide a query with index field" do
                let theQuery = query @Tag
                        |> innerJoin @Tagging (#id, #tagId)
                        |> innerJoinThirdTable @Post @Tagging (#id, #postId)
                        |> labelResults @Post #id
                (toSQL theQuery) `shouldBe` ("SELECT posts.id, tags.id, tags.tag_text FROM tags INNER JOIN taggings ON tags.id = taggings.tag_id INNER JOIN posts ON taggings.post_id = posts.id", [])



        describe "orderBy" do
            describe "orderByAsc" do
                it "should add a ORDER BY" do
                    let theQuery = query @Post
                            |> orderByAsc #createdAt

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts ORDER BY posts.created_at", [])
                
                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> orderByAsc #createdAt
                            |> orderByAsc #title

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts ORDER BY posts.created_at,posts.title", [])

            describe "orderByJoinedTable" do
                it "should add a ORDER BY" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByJoinedTable @User #name

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name", [])

                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByJoinedTable @User #name
                            |> orderByJoinedTable @User #id

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name,users.id", [])

            describe "orderByAscJoinedTable" do
                it "should add a ORDER BY" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByAscJoinedTable @User #name

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name", [])

                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByAscJoinedTable @User #name
                            |> orderByAscJoinedTable @User #id

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name,users.id", [])


            describe "orderByDesc" do
                it "should add a ORDER BY DESC" do
                    let theQuery = query @Post
                            |> orderByDesc #createdAt

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts ORDER BY posts.created_at DESC", [])
                
                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> orderByDesc #createdAt
                            |> orderByDesc #title

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts ORDER BY posts.created_at DESC,posts.title DESC", [])

            describe "orderByDescJoinedTable" do
                it "should add a ORDER BY" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByDescJoinedTable @User #name

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name DESC", [])

                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> innerJoin @User (#createdBy, #id)
                            |> orderByDescJoinedTable @User #name
                            |> orderByDescJoinedTable @User #id

                    (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts INNER JOIN users ON posts.created_by = users.id ORDER BY users.name DESC,users.id DESC", [])

        describe "limit" do
            it "should add a LIMIT" do
                let theQuery = query @Post
                        |> limit 1337

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts LIMIT 1337", [])
        
        describe "offset" do
            it "should add a OFFSET" do
                let theQuery = query @Post
                        |> offset 1337

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts OFFSET 1337", [])
        
        describe "queryOr" do
            it "should merge two conditions" do
                let theQuery = query @Post
                        |> queryOr
                            (filterWhere (#createdBy, "fe41a985-36a3-4f14-b13c-c166977dc7e8" :: UUID))
                            (filterWhere (#public, True))


                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE (posts.created_by = ?) OR (posts.public = ?)", [Plain "'fe41a985-36a3-4f14-b13c-c166977dc7e8'", Plain "true"])

        describe "distinct" do
            it "should add a DISTINCT" do
                let theQuery = query @Post
                        |> distinct

                (toSQL theQuery) `shouldBe` ("SELECT DISTINCT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts", [])
        
        describe "distinctOn" do
            it "should add a DISTINCT ON (..)" do
                let theQuery = query @Post
                        |> distinctOn #title

                (toSQL theQuery) `shouldBe` ("SELECT DISTINCT ON (posts.title) posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts", [])

        describe "Complex Queries" do
            it "should allow a query with limit and offset" do
                let theQuery = query @Post
                        |> offset 20
                        |> limit 50

                (toSQL theQuery) `shouldBe` ("SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts LIMIT 50 OFFSET 20", [])
            
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

                (toSQL theQuery) `shouldBe` (
                        "SELECT posts.id, posts.title, posts.external_url, posts.created_at, posts.public, posts.created_by, posts.category_id FROM posts WHERE (((posts.title = ?) AND (posts.public = ?)) AND (posts.external_url IS ?)) OR (posts.created_by = ?) ORDER BY posts.created_at,posts.title LIMIT 10",
                        [ Escape "test"
                        , Plain "true"
                        , Plain "null"
                        , Plain "'e70c66fb-68a5-41b8-8bf1-85b9bb046d15'"
                        ]
                    )
