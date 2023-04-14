{-|
Module: IHP.IDE.Defaults.TableColumnDefaults
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.IDE.Defaults.TableColumnDefaults where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.ControllerGenerator as ControllerGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport
import IHP.IDE.SchemaDesigner.Types


{- | Takes a 'Text' value for the name and creates a default column where all values are empty lists, 
'Nothing' or 'False'; and for 'ColumnType', the default is 'PUUID'.

Add a new field to the 'Column' type in the 
"IHP.IDE.SchemaDesigner.Types" file and then 
set its default value here

Defined as:

@
emptyColumn :: Column
emptyColumn = Column { 
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

@setColumnType pgt = emptyColumn {columnType = pgt}@

Or

Just as part of where you're calling it:

@someDefaultColumnType = emptyColumn {columnType = PDate}@
-}
emptyColumn :: Column
emptyColumn = 
      Column 
      { name = ""
      , columnType = PUUID 
      , defaultValue = Nothing
      , notNull = False 
      , isUnique = False
      , generator = Nothing
      }


{- | Creates an empty table with all values empty. @unlogged@ is set to 'False'.

Defined as such:

@
emptyTable :: CreateTable
emptyTable = CreateTable {
                        name = ""
                        , columns = []
                        , primaryKeyConstraint = PrimaryKeyConstraint []
                        , constraints = []
                        , unlogged = False
                        }
@

-}
emptyTable :: CreateTable
emptyTable = CreateTable 
                  { name = ""
                  , columns = []
                  , primaryKeyConstraint = PrimaryKeyConstraint []
                  , constraints = []
                  , unlogged = False
                  }

-- | Takes a name for our table and a list of column and inserts the list
--   into to our empty table.
defCreateTable :: Text -> [Column] -> CreateTable
defCreateTable tablename columns = emptyTable { name = tablename
                                              , columns = columns }




{- | Creates one default table with a singleton list of one 'setColumn'.

Uses both `defCreateTable` and `setColumn`.

@
defCreateTableWSetCol :: Text --  The name of the table
                        -> Text  --  The name of the column
                        -> PostgresType --  The type of the column
                        -> CreateTable -- The returned table
@

-}
defCreateTableWSetCol :: Text -> Text -> PostgresType -> CreateTable
defCreateTableWSetCol tablename columnname pgt = defCreateTable tablename [setColumn columnname pgt]

{- | Same as its progenitor `defCreateTableWSetCol` except it uses `setColumnN`
-}
defCreateTableWSetColN :: Text -> Text -> PostgresType -> CreateTable
defCreateTableWSetColN tablename columnname pgt = defCreateTable tablename [setColumnN columnname pgt]


{- | Takes the name of the table, the items you want inside the 'primaryKeyConstraint' and a list of columns
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
defCreateTablePKID name items cols = (defCreateTable name cols) {primaryKeyConstraint = PrimaryKeyConstraint items}


{- | Allows you to set the name and columnType. Uses `emptyColumn` as its base

If other values need to be changed, this can be done using: 
@(setColumn a b){..}@

__Example:__

>>> setColumn "user_id" PTrigger 
Column {name = "user_id", columnType = PTrigger, defaultValue = Nothing, notNull = False, isUnique = False, generator = Nothing}

-}
setColumn :: Text -> PostgresType -> Column
setColumn name pgt = emptyColumn { name = name
                               , columnType = pgt
                               }

-- | A version of `setColumn` where @notNull = True@
setColumnN :: Text -> PostgresType -> Column
setColumnN n p = (setColumn n p) {notNull = True}

-- | Sets a column to have a default value. Would recommend using in conjunction with `setColumn`
setColumnDefaultVal :: Maybe Expression -> Column -> Column
setColumnDefaultVal expression column = column {defaultValue = expression}


{- | A recurring unit as found in many tests and files.

Defined as such:

>>> idColumn
Column {name = "id", columnType = PUUID, defaultValue = Just (CallExpression "uuid_generate_v4" []), notNull = True, isUnique = False, generator = Nothing}

-}
idColumn :: Column
idColumn = setColumnDefaultVal (Just (CallExpression "uuid_generate_v4" [])) $ setColumnN "id" PUUID

{- | Give a column the text defined by 'text' and sets its 'columnType' to 'PText'. Uses `setColumnN`.

__Example:__

>>> colText "example"
Column {name = "example", columnType = PText, defaultValue = Nothing, notNull = True, isUnique = False, generator = Nothing}

-}
colText :: Text -> Column
colText text = setColumnN text PText


{- | A recurring table that appears in many tests.

This is its current definition:

@
compilerSpecTable :: CreateTable
compilerSpecTable = defCreateTablePKID "users" ["id"] cols

                  where cols = [ idColumn
                               , colText "firstname"
                               , colText "lastname"
                               , colText  "password_hash"
                               , colText "email"
                               , setColumnN "company_id" PUUID
                               , setColumn "picture_url" PText
                               , setColumnDefaultVal (Just (CallExpression "NOW" [])) $ setColumnN "created_at" PTimestampWithTimezone
                               ]
@
-}
compilerSpecTable :: CreateTable
compilerSpecTable = defCreateTablePKID "users" ["id"] cols

                  where cols = [ idColumn
                               , colText "firstname"
                               , colText "lastname"
                               , colText  "password_hash"
                               , colText "email"
                               , setColumnN "company_id" PUUID
                               , setColumn "picture_url" PText
                               , setColumnDefaultVal (Just (CallExpression "NOW" [])) $ setColumnN "created_at" PTimestampWithTimezone
                               ]


                                                
