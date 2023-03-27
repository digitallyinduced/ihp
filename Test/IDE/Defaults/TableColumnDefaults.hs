{-|
Module: Test.IDE.Defaults.TableColumnDefaults
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.Defaults.TableColumnDefaults where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport
import IHP.IDE.SchemaDesigner.Types


{- | Takes a `Text` value for the name and creates a default column where all values are empty lists, 
'Nothing' or 'False'; and for 'ColumnType', the default is 'PUUID'.

Add a new field to the 'Column' type in the 
"IHP.IDE.SchemaDesigner.Types" file and then 
set its default value here

Defined as:

@
defColumn :: Column
defColumn = Column { 
                 name = ""
               , columnType = PUUID 
               , defaultValue = Nothing
               , notNull = False 
               , isUnique = False
               , generator = Nothing
               }
@

If you want a different 'PostgresType' you will need to 
specify with like so by either using a function:

@setColumnType pgt = defColumn {columnType = pgt}@

Or

Just as part of where you're calling it:

@someDefaultColumnType = defColumn {columnType = PDate}@
-}
defColumn :: Column
defColumn = Column { 
                 name = ""
               , columnType = PUUID 
               , defaultValue = Nothing
               , notNull = False 
               , isUnique = False
               , generator = Nothing
               }


{- | Creates a table where all values are empty lists, including columns. @unlogged@ is set to 'False'.

Defined as such:

@
defCreateTable :: CreateTable
defCreateTable = CreateTable {
                        name = ""
                        , columns = []
                        , primaryKeyConstraint = PrimaryKeyConstraint []
                        , constraints = []
                        , unlogged = False
                        }
@

-}
defCreateTable :: Text -> CreateTable
defCreateTable t = CreateTable {
                        name = t
                        , columns = []
                        , primaryKeyConstraint = PrimaryKeyConstraint []
                        , constraints = []
                        , unlogged = False
                        }

-- | Takes a name for our table and a list of column and inserts the list
--  into to our default table.
defCreateTableWCol :: Text -> [Column] -> CreateTable
defCreateTableWCol t cols = (defCreateTable t) {columns = cols}

{- | Creates one default table with a singleton list of one 'setColumn'.

Uses both `defCreateTableWCol` and `setColumn`.

@
defCreateTableWSetCol :: Text --  The name of the table
                        -> Text  --  The name of the column
                        -> PostgresType --  The type of the column
                        -> CreateTable -- The returned table
@

-}
defCreateTableWSetCol :: Text -> Text -> PostgresType -> CreateTable
defCreateTableWSetCol tablename columnname pgt = defCreateTableWCol tablename (pure $ setColumn columnname pgt)

{- | Same as its progenitor `defCreateTableWSetCol` except it uses `setColumnN`
-}
defCreateTableWSetColN :: Text -> Text -> PostgresType -> CreateTable
defCreateTableWSetColN tablename columnname pgt = defCreateTableWCol tablename (pure $ setColumnN columnname pgt)


{- | Takes the name of the table, the items you want inside the primaryKeyConstraint and a list of columns
to return a table where the primary key constraint is set.

__Example:__ 

@ 
let orderTrucksTable = defCreateTablePKID "orderTrucks" ["order_id","truck_id"] cols
                  where cols = map mkColumn ["order_id","truck_id"]
                        mkColumn x = (setColumnN x PBigserial)
@

>>> orderTrucksTable
CreateTable { name = "orderTrucks"
            , columns = [ Column { name = "order_id", 
                                   columnType = PBigserial, 
                                   defaultValue = Nothing, 
                                   notNull = True, 
                                   isUnique = False, 
                                   generator = Nothing}
                        , Column { name = "truck_id", 
                                   columnType = PBigserial, 
                                   defaultValue = Nothing, 
                                   notNull = True, 
                                   isUnique = False, 
                                   generator = Nothing}
                         ]
            , primaryKeyConstraint = PrimaryKeyConstraint {primaryKeyColumnNames = ["order_id","truck_id"]}
            , constraints = []
            , unlogged = False}
-}
defCreateTablePKID :: Text -> [Text] -> [Column] -> CreateTable
defCreateTablePKID name items cols = (defCreateTableWCol name cols) {primaryKeyConstraint = PrimaryKeyConstraint items}


{- | Allows you to set the name and columnType. Uses `defColumn` as its base

If other values need to be changed, this can be done using: 
@(setColumn a b){..}@

__Example:__

>>> setColumn "user_id" PTrigger 
Column {name = "user_id", columnType = PTrigger, defaultValue = Nothing, notNull = False, isUnique = False, generator = Nothing}

-}
setColumn :: Text -> PostgresType -> Column
setColumn name pgt = defColumn { name = name
                               , columnType = pgt
                               }

-- | A version of `setColumn` where @notNull = True@
setColumnN :: Text -> PostgresType -> Column
setColumnN n p = (setColumn n p) {notNull = True}

-- | Sets a column to have a default value. Would recommend using in conjunction with `setColumn`
setColumnDefaultVal :: Maybe Expression -> Column -> Column
setColumnDefaultVal expression column = column {defaultValue = expression}


colUUID :: Column
colUUID = setColumnDefaultVal (Just (CallExpression "uuid_generate_v4" [])) $ setColumnN "id" PUUID

colText :: Text -> Column
colText t = setColumnN t PText

colName :: Column
colName = colText "name"

colFName :: Column
colFName = colText "firstname"

colLName :: Column
colLName = colText "lastname"

colEmail :: Column
colEmail = colText "email"

colHash :: Column
colHash = colText  "password_hash"

colCompanyID :: Column
colCompanyID = (colText "company_id") { columnType = PUUID }

colPicUrl :: Column
colPicUrl = setColumn "picture_url" PText

colCreatedAt :: Column
colCreatedAt = setColumnDefaultVal (Just (CallExpression "NOW" [])) $ setColumnN "created_at" PTimestampWithTimezone

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
colExampleCont = (colText "content") { defaultValue = Just (TextExpression "example text")
                                     }

pagesTable :: CreateTable
pagesTable = (defCreateTableWCol "pages" [colUUID]) 
                {  primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                }

peopleTable :: CreateTable
peopleTable = (defCreateTableWCol "people" [colUUID, colName, colEmail]) 
                   { primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                   , unlogged = False
                   }

mailTable :: CreateTable
mailTable = (defCreateTableWCol "users" [colUUID]) { primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                                                   }

compilerSpecTable :: CreateTable
compilerSpecTable = (defCreateTableWCol "users" [colUUID
                                       , colFName
                                       , colLName
                                       , colHash
                                       , colEmail
                                       , colCompanyID
                                       , colPicUrl
                                       , colCreatedAt
                                       ]) 
                     { primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                     }

productTable :: CreateTable
productTable = (defCreateTableWCol "products" [colTs])

quotedNameTable :: CreateTable
quotedNameTable = defCreateTable "quoted name"

deprecVarTable :: CreateTable
deprecVarTable = (defCreateTableWCol "deprecated_variables" depVars)
            where depVars = [a,b,c,d]
                  a = setColumn "a" (PNumeric Nothing Nothing)
                  b = setColumn "b" (PNumeric (Just 1) Nothing)
                  c = setColumn "c" (PNumeric (Just 1) (Just 2))
                  d = setColumn "d" (PVaryingN (Just 10))


followerTable :: CreateTable 
followerTable = (defCreateTableWCol "user_followers" followFields) 
                                                  { primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                                                  , constraints = [ UniqueConstraint { name = Nothing, columnNames = [ "user_id", "follower_id" ] } ]
                                                  }
               where followFields = [colUUID, user_id, follower_id]
                     user_id = setColumnN "user_id" PUUID
                     follower_id = setColumnN "follower_id" PUUID

intTable :: CreateTable
intTable = defCreateTableWCol "ints" intCols
        
         where intCols = map mkPintCol ["int_a","int_b","int_c"] 
                         <> map mkPSmallInt ["smallint_a","smallint_b"]
                         <> map mkBigInt ["bigint_a","bigint_b"]

               mkPintCol x = setColumn x PInt
               mkPSmallInt x = setColumn x PSmallInt
               mkBigInt x = setColumn x PBigInt

timestampTable :: CreateTable
timestampTable = defCreateTableWCol "timestamps" ts
               
               where ts            = map mkTimeStamp ["a","b"]
                     mkTimeStamp x = setColumn x PTimestampWithTimezone

boolTable :: CreateTable
boolTable = defCreateTableWCol "bools" bs
               
               where bs       = map mkBool ["a","b"]
                     mkBool :: Text -> Column
                     mkBool x = setColumn x PBoolean

realFloatTable :: CreateTable
realFloatTable = defCreateTableWCol "realfloat" (reals <> doubles)
            where reals = map mkReal ["a","b"]
                  doubles = map mkDouble ["c","d"]
                  mkReal x   = setColumn x PReal
                  mkDouble x = setColumn x PDouble

userFollowerTable :: CreateTable
userFollowerTable = (defCreateTableWCol "user_followers" fields) 
                                                { primaryKeyConstraint = PrimaryKeyConstraint [ "user_id", "follower_id" ]
                                                }
                     
                     where fields = map mkField ["user_id","follower_id"]
                           mkField x = defColumn { name = x
                                                 , notNull = True } :: Column


ordersSerialTable :: CreateTable
ordersSerialTable = (defCreateTableWCol "orders" serCol) { primaryKeyConstraint = PrimaryKeyConstraint ["id"] }

                     where serCol = [ (setColumnN "id" PSerial)
                                    ]

ordersBigSerialTable :: CreateTable
ordersBigSerialTable = ordersSerialTable {columns = bigSerCol} 

                     where bigSerCol = [ (setColumnN "id" PBigserial)
                                       ]

orderTrucksTable :: CreateTable
orderTrucksTable = defCreateTablePKID "orderTrucks" ["order_id","truck_id"] cols
                  where cols = map mkColumn ["order_id","truck_id"]
                        mkColumn x = (setColumnN x PBigserial)

arrayTestTable :: CreateTable
arrayTestTable = defCreateTableWCol "array_tests" arrayCol
                  where arrayCol = pure $ setColumn "pay_by_quarter" (PArray PInt)

pointsTable :: CreateTable
pointsTable = defCreateTableWCol "points" pointCol
                  where pointCol = pure $ setColumn "pos" PPoint

polygonTable :: CreateTable
polygonTable = defCreateTableWCol "polygons" polyCol
                  where polyCol = pure $ setColumn "poly" PPolygon

electricityTableD :: CreateTable
electricityTableD = defCreateTableWCol "a" eupCol
                  where eupCol = pure $ 
                              setColumnDefaultVal (Just (TypeCastExpression (DoubleExpression 0.17) PDouble) ) $ 
                                    setColumnN "electricity_unit_price" PDouble


electricityTableI :: CreateTable
electricityTableI = defCreateTableWCol "a" eupCol
                  where eupCol = pure . setColumnDefaultVal (Just (IntExpression 0)) $ (setColumnN "electricity_unit_price" PInt)


typeCastTable :: CreateTable
typeCastTable = defCreateTableWCol "a" tcCol
                  where init  = setColumn "a" (PVaryingN (Just 510))
                        def   = Just (TypeCastExpression (VarExpression "NULL") (PVaryingN Nothing))
                        tcCol = pure $ setColumnDefaultVal def init

emptyBinaryTable :: CreateTable
emptyBinaryTable = defCreateTableWCol "a" ebCol
                  where init  = setColumnN "a" PBinary
                        def   = Just (TypeCastExpression (TextExpression "") PBinary)
                        ebCol = pure $ setColumnDefaultVal def init

publicVariablesTable :: CreateTable
publicVariablesTable = defCreateTableWCol "public_variables" idCol
                        where idCol = pure $ setColumn "id" PUUID

notifTable :: CreateTable
notifTable = (defCreateTable "pg_large_notifications") {unlogged = True}


postUserTable :: CreateTable
postUserTable = defCreateTableWSetCol "posts" "user_id" PUUID

postTitleTable :: CreateTable
postTitleTable = defCreateTableWCol "posts" [(colText "title")]

pointTestTable :: CreateTable
pointTestTable = defCreateTableWSetCol "point_tests" "pos" PPoint

polyTestTable :: CreateTable
polyTestTable = defCreateTableWSetCol "polygon_tests" "poly" PPolygon

tableAWithCreatedAtTable :: CreateTable
tableAWithCreatedAtTable  = defCreateTableWCol "a" updatedAtCol
                        where updatedAtCol = pure $ 
                                                setColumnDefaultVal (Just (CallExpression "NOW" ([]))) $ 
                                                setColumnN "updated_at" PTimestampWithTimezone

ihpuserTable :: CreateTable
ihpuserTable = defCreateTableWCol "a" ihpuser
                                where ihpuser = pure $ 
                                        setColumnDefaultVal (Just (CallExpression "ihp_user_id" ([]))) $
                                        setColumn "user_id" PUUID
