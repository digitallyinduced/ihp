{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module Test.Defaults.CreateTableDefaults where

import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types

-- May be worth bringing in Control.Lens to make getting and setting nested fields a bit better, 
-- currently learning how to do that.

{- | Creates a default column where all values are Empty, Nothing or False; and for ColumnType, the default is
PUUID.

If you want a different ColumnType PostGresType you will need to specify with like so by either using a function:

@
setColumnType pgt = defColumn {postGresType = pgt}
@

Or

Just as part of where you're calling it:

@
someDefaultColumnType = defColumn {columnType = PDate}
@
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


-- | Creates a table where all values are empty lists, including columns. @unlogged@ is set to @True@.
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

-- | Creates one default table with a singleton list of one @defColumn@ .
defCreateTableWDefCol :: CreateTable
defCreateTableWDefCol = defCreateTableWCol (pure defColumn)