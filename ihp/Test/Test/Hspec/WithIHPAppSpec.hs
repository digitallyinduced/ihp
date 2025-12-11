{-|
Module: Test.Hspec.WithIHPAppSpec
Copyright: (c) digitally induced GmbH, 2024
Description: Test coverage for withIHPApp function
-}
module Test.Hspec.WithIHPAppSpec where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.Test.Mocking
import Test.Hspec
import IHP.ControllerSupport
import IHP.ModelSupport
import IHP.QueryBuilder
import IHP.Fetch
import Network.HTTP.Types.Status
import Network.Wai
import IHP.Hspec (withIHPApp)

-- Define a minimal test application
data TestApplication = TestApplication deriving (Eq, Show)

instance InitControllerContext TestApplication where
    initContext = pure ()

-- Define a test data type that matches our Schema.sql
data Post = Post
    { id :: !(Id Post)
    , title :: !Text
    , body :: !Text
    , createdAt :: !UTCTime
    } deriving (Eq, Show)

instance HasField "id" Post (Id Post) where
    getField post = post.id

type instance GetTableName Post = "posts"
type instance GetModelByTableName "posts" = Post
type instance PrimaryKey "posts" = UUID

instance Record Post where
    newRecordInstance = Post def "" "" def

-- Define a minimal config
testConfig :: ConfigBuilder
testConfig = do
    option Development
    option (AppHostname "localhost")

tests :: Spec
tests = aroundAll (withIHPApp TestApplication testConfig) do
    describe "withIHPApp" do
        it "can initialize a test context" $ withContext do
            -- This test verifies that withIHPApp properly initializes the context
            pure () :: IO ()

        it "can access the database" $ withContext do
            -- This test verifies that database access works through withIHPApp
            count <- query @Post |> fetchCount
            count `shouldBe` 0

        it "can create and query records" $ withContext do
            -- Create a test post
            post <- newRecord @Post
                |> set #title "Test Title"
                |> set #body "Test Body"
                |> createRecord

            -- Verify it was created
            count <- query @Post |> fetchCount
            count `shouldBe` 1

            -- Fetch and verify the post
            fetchedPost <- query @Post |> fetchOne
            fetchedPost.title `shouldBe` "Test Title"
            fetchedPost.body `shouldBe` "Test Body"

        it "provides isolated database context per test" $ withContext do
            -- This test should start with a clean database
            -- even though the previous test created a record
            count <- query @Post |> fetchCount
            count `shouldBe` 0
