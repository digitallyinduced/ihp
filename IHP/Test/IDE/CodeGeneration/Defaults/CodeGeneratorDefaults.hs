{-|
Module: Test.IDE.CodeGeneration.ControllerGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.CodeGeneration.Defaults.CodeGeneratorDefaults where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport
import Test.DefaultValues.CreateTableDefaults

colUUID :: Column
colUUID = defColumn { name = "id"
                 , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                 , notNull = True
                 }


colName :: Column
colName = defColumn { name = "name"
                    , columnType = PText
                    , notNull = True
                    }

colEmail :: Column
colEmail = defColumn { name = "email"
                     , columnType = PText
                     , notNull = True
                     }
pagesTable :: CreateTable
pagesTable = (defCreateTableWCol [colUUID]) 
                { name = "pages"
                , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                , unlogged = False
                }

peopleTable :: CreateTable
peopleTable = (defCreateTableWCol [colUUID, colName, colEmail]) 
                {  name = "people"
                   , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                   , unlogged = False
                }

mailTable :: CreateTable
mailTable = (defCreateTableWCol [colUUID]){
                        name = "users"
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }