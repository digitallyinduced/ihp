module Test.IDE.SchemaDesigner.Controller.EnumValuesSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Controller.EnumValues
import IHP.IDE.SchemaDesigner.Types

tests :: SpecWith ()
tests = do
    describe "IHP.IDE.SchemaDesigner.Controller.EnumValues" do
        describe "getAllEnumValues" do
            it "should return a list of all enum values" do
                getAllEnumValues [] `shouldBe` []
                getAllEnumValues [ CreateExtension { name ="a", ifNotExists = True } ] `shouldBe` []
                getAllEnumValues [ CreateEnumType { name = "first_enum", values=["a", "b", "c"] }] `shouldBe` ["a", "b", "c"]
                getAllEnumValues
                    [ CreateEnumType {name = "first_enum", values = ["a", "b"]}
                    , CreateExtension {name = "extension", ifNotExists = True}
                    , CreateEnumType {name = "second_enum", values = ["c"]}
                    , CreateEnumType {name = "third_enum", values = []}
                    ]
                    `shouldBe` ["a","b","c"]
