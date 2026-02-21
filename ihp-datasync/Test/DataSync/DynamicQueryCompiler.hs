{-|
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.DataSync.DynamicQueryCompiler where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.DynamicQuery
import IHP.QueryBuilder hiding (OrderByClause)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as Aeson

-- | Extract SQL text from a CompiledQuery for testing purposes.
compiledQueryToSql :: CompiledQuery -> Text
compiledQueryToSql (CompiledQuery sql _) = sql

-- | Column types for the "posts" table used in tests.
-- Simulates database column order: id first, then other columns in schema definition order.
postsTypes :: ColumnTypeInfo
postsTypes = ColumnTypeInfo
    { typeMap = HashMap.fromList
        [ ("user_id", "uuid")
        , ("id", "uuid")
        , ("a", "text")
        , ("title", "text")
        , ("ts", "tsvector")
        , ("group_id", "uuid")
        ]
    , orderedColumns = ["id", "user_id", "title", "a", "ts", "group_id"]
    }

-- | Column types for the "products" table used in tests.
productsTypes :: ColumnTypeInfo
productsTypes = ColumnTypeInfo
    { typeMap = HashMap.fromList [("ts", "tsvector")]
    , orderedColumns = ["ts"]
    }

-- | Compile a query with the camelCase renamer and typed encoding.
compile :: ColumnTypeInfo -> DynamicSQLQuery -> CompiledQuery
compile = compileQueryTyped camelCaseRenamer

-- | Expected SELECT clause for postsTypes when using SelectAll
-- Columns appear in database schema order (from orderedColumns), with camelCase aliases
postsSelectAll :: Text
postsSelectAll = "\"id\", \"user_id\" AS \"userId\", \"title\", \"a\", \"ts\", \"group_id\" AS \"groupId\""

tests = do
    describe "IHP.DataSync.DynamicQueryCompiler" do
        describe "compileQueryTyped" do
            it "compile a basic select query" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                -- SelectAll expands to all columns with appropriate camelCase aliases (id first, then alphabetically)
                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\"")

            it "compile a select query with order by" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , distinctOnColumn = Nothing
                        , orderByClause = [OrderByClause { orderByColumn = "title", orderByDirection = Desc }]
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" ORDER BY \"title\" DESC")

            it "compile a select query with multiple order bys" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = [
                            OrderByClause { orderByColumn = "createdAt", orderByDirection = Desc },
                            OrderByClause { orderByColumn = "title", orderByDirection = Asc }
                        ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" ORDER BY \"created_at\" DESC, \"title\"")

            it "compile a basic select query with a where condition" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (Aeson.String "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") = ($1)")

            it "compile a basic select query with a where condition and an order by" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (Aeson.String "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = [ OrderByClause { orderByColumn = "createdAt", orderByDirection = Desc } ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") = ($1) ORDER BY \"created_at\" DESC")

            it "compile a basic select query with a limit" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (Aeson.String "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , distinctOnColumn = Nothing
                        , orderByClause = []
                        , limit = Just 50
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") = ($1) LIMIT $2")

            it "compile a basic select query with an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (Aeson.String "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Just 50
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") = ($1) OFFSET $2")

            it "compile a basic select query with a limit and an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (Aeson.String "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Just 25
                        , offset = Just 50
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") = ($1) LIMIT $2 OFFSET $3")

            it "compile 'field = NULL' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression Aeson.Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") IS NULL")

            it "compile 'field <> NULL' conditions to 'field IS NOT NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpNotEqual (LiteralExpression Aeson.Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"user_id\") IS NOT NULL")

            it "compile 'field IN (NULL)' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Aeson.Null] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"a\") IS NULL")

            it "compile 'field IN (NULL, 'string')' conditions to 'field IS NULL OR field IN ('string')'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Aeson.Null, Aeson.String "test" ] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE ((\"a\") IN ($1)) OR ((\"a\") IS NULL)")

            it "compile queries with TS expressions" do
                let query = DynamicSQLQuery
                        { table = "products"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "ts") OpTSMatch (CallExpression { functionCall = ToTSQuery { text = "test" }})
                        , orderByClause = [ OrderByTSRank { tsvector = "ts", tsquery = "test" } ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                -- productsTypes only has "ts" column (no rename needed since it's already snake_case = camelCase)
                compiledQueryToSql (compile productsTypes query) `shouldBe`
                        "SELECT \"ts\" FROM \"products\" WHERE (\"ts\") @@ (to_tsquery('english', $1)) ORDER BY ts_rank(\"ts\", to_tsquery('english', $2))"

            it "compile a basic select query with distinctOn" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = []
                        , distinctOnColumn = Just "groupId"
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT DISTINCT ON (\"group_id\") " <> postsSelectAll <> " FROM \"posts\"")

            it "compile a WHERE IN query" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "id") OpIn (ListExpression { values = [Aeson.String "a5d7772f-c63f-4444-be69-dd9afd902e9b", Aeson.String "bb88d55a-1ed0-44ad-be13-d768f4b3f9ca"] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE (\"id\") IN ($1, $2)")

            it "compile an empty WHERE IN query to FALSE" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "id") OpIn (ListExpression { values = [] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        ("SELECT " <> postsSelectAll <> " FROM \"posts\" WHERE FALSE")

            it "compile SelectSpecific with camelCase to snake_case aliases" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectSpecific ["userId", "title"]
                        , whereCondition = Nothing
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                -- SelectSpecific columns get renamed to snake_case and aliased back to camelCase
                compiledQueryToSql (compile postsTypes query) `shouldBe`
                        "SELECT \"user_id\" AS \"userId\", \"title\" FROM \"posts\""
