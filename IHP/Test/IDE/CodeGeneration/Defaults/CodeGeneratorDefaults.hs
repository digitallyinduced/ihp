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


colCust :: Column
colCust = defColumn {columnType = PCustomType ""}

colCustT :: PostgresType -> Column
colCustT t = defColumn {columnType = t}

colText :: Column
colText = defColumn { columnType = PText
                    , notNull = True
                    }

colName :: Column
colName = colText { name = "name"}

colFName :: Column
colFName = colText { name = "firstname"}

colLName :: Column
colLName = colText { name = "lastname"}

colEmail :: Column
colEmail = colText { name = "email"}

colHash :: Column
colHash = colText { name = "password_hash"}

colCompanyID :: Column
colCompanyID = colText { name = "company_id"
                       , columnType = PUUID
                       }

colPicUrl :: Column
colPicUrl = defColumn {columnType = PUUID}

colCreatedAt :: Column
colCreatedAt = defColumn { name = "created_at"
                         , columnType = PTimestampWithTimezone
                         , defaultValue = Just (CallExpression "NOW" [])
                         , notNull = True
                         }
                         

colTs :: Column
colTs = defColumn { name = "ts"
                  , columnType = PTSVector
                  , generator = Just $ ColumnGenerator
                                        { generate =
                                            ConcatenationExpression
                                                (ConcatenationExpression
                                                    (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "sku"],TextExpression "A"])
                                                    (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "name"],TextExpression "B"])
                                                )
                                                (CallExpression "setweight" [CallExpression "to_tsvector" [TextExpression "english",VarExpression "description"],TextExpression "C"])
                                        , stored = True
                                        }
                  }

colExampleCont :: Column
colExampleCont = colText { name = "content"
                         , defaultValue = Just (TextExpression "example text")
                         }

pagesTable :: CreateTable
pagesTable = (defCreateTableWCol [colUUID]) 
                { name = "pages"
                , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                }

peopleTable :: CreateTable
peopleTable = (defCreateTableWCol [colUUID, colName, colEmail]) 
                   { name = "people"
                   , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                   , unlogged = False
                   }

mailTable :: CreateTable
mailTable = (defCreateTableWCol [colUUID]){
                        name = "users"
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        }

compilerSpecTable :: CreateTable
compilerSpecTable = (defCreateTableWCol [colUUID
                                       , colFName
                                       , colLName
                                       , colHash
                                       , colEmail
                                       , colCompanyID
                                       , colPicUrl
                                       , colCreatedAt
                                       ]) 
                     { name = "users"
                     , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                     }

productTable :: CreateTable
productTable = (defCreateTableWCol [colTs])
                { name = "products"
                }

quotedNameTable :: CreateTable
quotedNameTable = defCreateTable { name="quoted name"}

deprecVarTable :: CreateTable
deprecVarTable = (defCreateTableWCol depVars) {name = "deprecated_variables"}
            where depVars = [a,b,c,d]
                  a = defColumn { name = "a" 
                                , columnType = (PNumeric Nothing Nothing)
                                }

                  b = defColumn { name = "b" 
                                , columnType = (PNumeric (Just 1) Nothing)
                                }

                  c = defColumn { name = "c" 
                                , columnType = (PNumeric (Just 1) (Just 2))
                                }

                  d = defColumn { name = "d" 
                                , columnType = (PVaryingN (Just 10))
                                }


followerTable :: CreateTable 
followerTable = (defCreateTableWCol followFields) { name = "user_followers"
                                                  , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                                                  , constraints = [ UniqueConstraint { name = Nothing, columnNames = [ "user_id", "follower_id" ] } ]
                                                  }
               where followFields = [colUUID, user_id, follower_id]
                     user_id = defColumn {name = "user_id", notNull = True}
                     follower_id = defColumn {name = "follower_id", notNull = True}

intTable :: CreateTable
intTable = (defCreateTableWCol intCols) { name = "ints" }
        
         where intCols = map mkPintCol ["int_a","int_b","int_c"] 
                         <> map mkPSmallInt ["smallint_a","smallint_b"]
                         <> map mkBigInt ["bigint_a","bigint_b"]

               mkPintCol x = colCust { name = x
                                      , columnType = PInt}
               mkPSmallInt x = colCust { name = x
                                        , columnType = PSmallInt}
               mkBigInt x = colCust { name = x
                                      , columnType = PBigInt}

timestampTable :: CreateTable
timestampTable = (defCreateTableWCol ts) {name = "timestamps"}
               
               where ts            = map mkTimeStamp ["a","b"]
                     mkTimeStamp x = (colCustT PTimestampWithTimezone) { name = x } :: Column

boolTable :: CreateTable
boolTable = (defCreateTableWCol bs) {name = "bools"}
               
               where bs       = map mkBool ["a","b"]
                     mkBool :: Text -> Column
                     mkBool x = (colCustT PBoolean) { name = x } 

realFloatTable :: CreateTable
realFloatTable = (defCreateTableWCol (reals <> doubles)) {name = "realfloat"}
            where reals = map mkReal ["a","b"]
                  doubles = map mkDouble ["c","d"]
                  mkReal x   = (colCustT PBoolean) { name = x} :: Column
                  mkDouble x = (colCustT PDouble) { name = x} :: Column

userFollowerTable :: CreateTable
userFollowerTable = (defCreateTableWCol fields) { name = "user_followers"
                                                , primaryKeyConstraint = PrimaryKeyConstraint [ "user_id", "follower_id" ]
                                                }
                     
                     where fields = map mkField ["user_id","follower_id"]
                           mkField x = defColumn { name = x
                                                 , notNull = True } :: Column


ordersSerialTable :: CreateTable
ordersSerialTable = (defCreateTableWCol serCol) { primaryKeyConstraint = PrimaryKeyConstraint ["id"] }

                     where serCol = [ (colCustT PSerial) { name = "orders"
                                                         , notNull = True
                                                         }
                                    ]

ordersBigSerialTable :: CreateTable
ordersBigSerialTable = ordersSerialTable {columns = bigSerCol} 

                     where bigSerCol = [ (colCustT PBigserial) { name = "orders"
                                                               , notNull = True
                                                               }
                                       ]