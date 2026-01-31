{-|
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.DataSync.DynamicQueryCompiler where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.DynamicQuery
import IHP.QueryBuilder hiding (OrderByClause)
import qualified Hasql.DynamicStatements.Snippet as Snippet
import Hasql.Statement (Statement(..))
import qualified Hasql.DynamicStatements.Statement as DynStatement
import qualified Hasql.Decoders as Decoders
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Data.HashMap.Strict as HashMap

-- | Convert a Snippet to its SQL text representation for testing purposes.
snippetToSql :: Snippet -> ByteString
snippetToSql snippet = case DynStatement.dynamicallyParameterized snippet Decoders.noResult False of
    Statement sql _ _ _ -> sql

-- | Column types for the "posts" table used in tests.
postsTypes :: ColumnTypeMap
postsTypes = HashMap.fromList
    [ ("user_id", "uuid")
    , ("id", "uuid")
    , ("a", "text")
    , ("title", "text")
    , ("ts", "tsvector")
    , ("group_id", "uuid")
    ]

-- | Column types for the "products" table used in tests.
productsTypes :: ColumnTypeMap
productsTypes = HashMap.fromList
    [ ("ts", "tsvector")
    ]

-- | Compile a query with the camelCase renamer and typed encoding.
compile :: ColumnTypeMap -> DynamicSQLQuery -> Snippet
compile = compileQueryTyped camelCaseRenamer

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

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\""

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

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" ORDER BY \"title\" DESC"

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

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" ORDER BY \"created_at\" DESC, \"title\""

            it "compile a basic select query with a where condition" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") = ($1)"

            it "compile a basic select query with a where condition and an order by" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = [ OrderByClause { orderByColumn = "createdAt", orderByDirection = Desc } ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") = ($1) ORDER BY \"created_at\" DESC"

            it "compile a basic select query with a limit" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , distinctOnColumn = Nothing
                        , orderByClause = []
                        , limit = Just 50
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") = ($1) LIMIT $2"

            it "compile a basic select query with an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Just 50
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") = ($1) OFFSET $2"

            it "compile a basic select query with a limit and an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Just 25
                        , offset = Just 50
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") = ($1) LIMIT $2 OFFSET $3"

            it "compile 'field = NULL' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") IS NULL"

            it "compile 'field <> NULL' conditions to 'field IS NOT NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpNotEqual (LiteralExpression Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"user_id\") IS NOT NULL"

            it "compile 'field IN (NULL)' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Null] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"a\") IS NULL"

            it "compile 'field IN (NULL, 'string')' conditions to 'field IS NULL OR field IN ('string')'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Null, TextValue "test" ] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE ((\"a\") IN ($1)) OR ((\"a\") IS NULL)"

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

                snippetToSql (compile productsTypes query) `shouldBe`
                        "SELECT * FROM \"products\" WHERE (\"ts\") @@ (to_tsquery('english', $1)) ORDER BY ts_rank(\"ts\", to_tsquery('english', $2))"

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

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT DISTINCT ON (\"group_id\") * FROM \"posts\""

            it "compile a WHERE IN query" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "id") OpIn (ListExpression { values = [UUIDValue "a5d7772f-c63f-4444-be69-dd9afd902e9b", UUIDValue "bb88d55a-1ed0-44ad-be13-d768f4b3f9ca"] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE (\"id\") IN ($1, $2)"

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

                snippetToSql (compile postsTypes query) `shouldBe`
                        "SELECT * FROM \"posts\" WHERE FALSE"
