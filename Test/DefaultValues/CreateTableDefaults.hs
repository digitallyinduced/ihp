{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Test.DefaultValues.CreateTableDefaults where
{- | This module aims to create some default tables and columns for tables for use 
elsewhere and make it easier to add fields elsewhere whenever necessary.
-}
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types


{- | Creates a default column where all values are empty lists/text, 
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
defCreateTable :: CreateTable
defCreateTable = CreateTable {
                        name = ""
                        , columns = []
                        , primaryKeyConstraint = PrimaryKeyConstraint []
                        , constraints = []
                        , unlogged = False
                        }

-- | Takes a list of column and adds it to our default table.
defCreateTableWCol :: [Column] -> CreateTable
defCreateTableWCol cols = defCreateTable {columns = cols}

-- | Creates one default table with a singleton list of one 'defColumn' .
defCreateTableWDefCol :: CreateTable
defCreateTableWDefCol = defCreateTableWCol (pure defColumn)