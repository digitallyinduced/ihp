module Test.IDE.SchemaDesigner.Controller.HelperSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Controller.Helper
import IHP.IDE.SchemaDesigner.Types

tests :: SpecWith ()
tests = do
    describe "IHP.IDE.SchemaDesigner.Controller.Helper" do
        describe "getAllObjectNames" do
            it "should return a list of all names of tables and enum types" do
                getAllObjectNames [] `shouldBe` []
                getAllObjectNames [ CreateExtension { name ="a", ifNotExists = True } ] `shouldBe` []
                getAllObjectNames [ CreateEnumType { name = "first_enum", values=["a", "b", "c"] }] `shouldBe` ["first_enum"]
                getAllObjectNames [ StatementCreateTable CreateTable
                                        { name = "table_name", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints=[]}
                                  ]
                    `shouldBe` ["table_name"]
                getAllObjectNames
                    [ CreateEnumType {name = "first_enum", values = ["a", "b"]}
                    , CreateExtension {name = "extension", ifNotExists = True}
                    , StatementCreateTable CreateTable
                        { name = "table_name", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints=[]}
                    , CreateEnumType {name = "second_enum", values = []}
                    ]
                    `shouldBe` ["first_enum","table_name","second_enum"]
