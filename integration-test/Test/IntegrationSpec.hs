module Test.IntegrationSpec where

import Web.Controller.Prelude hiding (get, request)
import IHP.FrameworkConfig
import IHP.Environment
import IHP.Test.Mocking
import IHP.Hspec (withIHPApp)
import Test.Hspec

import Web.FrontController ()

testConfig :: ConfigBuilder
testConfig = do
    option Development
    option (AppPort 8000)

tests :: Spec
tests = around (withIHPApp WebApplication testConfig) do
    describe "withIHPApp Integration" do
        it "can create and query records" $ withContext do
            user <- newRecord @User
                |> set #email "test@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            post <- newRecord @Post
                |> set #title "Test Post"
                |> set #body "Hello World"
                |> set #userId user.id
                |> createRecord

            posts <- query @Post |> fetch
            length posts `shouldBe` 1
            let [thePost] = posts
            thePost.title `shouldBe` "Test Post"

        it "can update a record" $ withContext do
            user <- newRecord @User
                |> set #email "update@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            post <- newRecord @Post
                |> set #title "Original Title"
                |> set #body "Body"
                |> set #userId user.id
                |> createRecord

            post
                |> set #title "Updated Title"
                |> updateRecord

            updatedPost <- fetch post.id
            updatedPost.title `shouldBe` "Updated Title"

        it "can delete a record" $ withContext do
            user <- newRecord @User
                |> set #email "delete@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            post <- newRecord @Post
                |> set #title "To Delete"
                |> set #body "Body"
                |> set #userId user.id
                |> createRecord

            deleteRecord post

            posts <- query @Post |> fetch
            length posts `shouldBe` 0

        it "can call a controller action" $ withContext do
            user <- newRecord @User
                |> set #email "ctrl@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            response <- callActionWithParams CreatePostAction
                [ ("title", "From Controller")
                , ("body", "Test body")
                , ("userId", cs $ show user.id)
                ]

            posts <- query @Post |> fetch
            length posts `shouldBe` 1
            let [thePost] = posts
            thePost.title `shouldBe` "From Controller"
