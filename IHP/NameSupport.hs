{-|
Module: IHP.NameSupport
Description:  Transforms names, e.g. table names to model names
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.NameSupport
( tableNameToModelName
, columnNameToFieldName
, modelNameToTableName
, humanize
, ucfirst
, lcfirst
, fieldNameToColumnName
, escapeHaskellKeyword
, tableNameToControllerName
) where

import Prelude hiding (splitAt)
import IHP.HaskellSupport
import Data.Text
import Data.String.Conversions (cs)
import qualified Text.Inflections as Inflector
import qualified Text.Countable as Countable

-- | Transforms a underscore table name to a camel case model name.
--
-- >>> tableNameToModelName "users"
-- "User"
--
-- >>> tableNameToModelName "projects"
-- "Project"
tableNameToModelName :: Text -> Text
tableNameToModelName "brain_waves" = "BrainWave"
tableNameToModelName tableName = do
    let singularizedTableName = cs (Countable.singularize tableName)
    if "_" `isInfixOf` singularizedTableName 
        then unwrapEither tableName $ Inflector.toCamelCased True $ singularizedTableName
        else ucfirst singularizedTableName
{-# INLINE tableNameToModelName #-}

-- | Transforms a underscore table name to a name for a contorller
--
-- >>> tableNameToControllerName "users"
-- "Users"
--
-- >>> tableNameToControllerName "projects"
-- "Projects"
--
-- >>> tableNameToControllerName "user_projects"
-- "UserProjects"
tableNameToControllerName :: Text -> Text
tableNameToControllerName tableName = do
    if "_" `isInfixOf` tableName 
        then unwrapEither tableName $ Inflector.toCamelCased True tableName
        else ucfirst tableName
{-# INLINE tableNameToControllerName #-}

-- | Transforms a camel case model name to a underscored table name.
--
-- >>> modelNameToTableName "User"
-- "users"
--
-- >>> modelNameToTableName "UserProject"
-- "user_projects"
modelNameToTableName :: Text -> Text
modelNameToTableName modelName =
        Inflector.toUnderscore modelName
        |> unwrapEither modelName
        |> Countable.pluralize
{-# INLINE modelNameToTableName #-}

-- | Transforms a underscore table column name to a camel case attribute name for use in haskell.
--
-- >>> columnNameToFieldName "email"
-- "email"
--
-- >>> columnNameToFieldName "project_id"
-- "projectId"
columnNameToFieldName :: Text -> Text
columnNameToFieldName columnName = escapeHaskellKeyword (unwrapEither columnName $ Inflector.toCamelCased False columnName)
{-# INLINE columnNameToFieldName #-}

{-# INLINE unwrapEither #-}
unwrapEither _ (Right value) = value
unwrapEither input (Left value) = error ("IHP.NameSupport: " <> show value <> " (value to be transformed: " <>  show input <> ")")

-- | Transforms a camel case attribute name from haskell to a underscore table column name for the database.
--
-- >>> fieldNameToColumnName "email"
-- "email"
--
-- >>> fieldNameToColumnName "projectId"
-- "project_id"
fieldNameToColumnName :: Text -> Text
fieldNameToColumnName columnName = unwrapEither columnName $ Inflector.toUnderscore columnName
{-# INLINE fieldNameToColumnName #-}

-- | Returns a more friendly version for an identifier
humanize :: Text -> Text
humanize text = unwrapEither text $ Inflector.toHumanized True text
{-# INLINE humanize #-}

{-# INLINE applyFirst #-}
applyFirst :: (Text -> Text) -> Text -> Text
applyFirst f text =
    let (first, rest) = splitAt 1 text
    in (f first) <> rest

-- | Make a text's first character lowercase
--
-- >>> lcfirst "Hello World"
-- "hello World"
--
-- >>> lcfirst "alread lowercase"
-- "already lowercase"
lcfirst :: Text -> Text
lcfirst = applyFirst toLower
{-# INLINE lcfirst #-}

-- | Make a text's first character uppercase
--
-- >>> ucfirst "hello world"
-- "Hello World"
--
-- >>> ucfirst "Already uppercase"
-- "Already uppercase"
ucfirst :: Text -> Text
ucfirst = applyFirst toUpper
{-# INLINE ucfirst #-}

-- | Add '_' to the end of a name if it is a reserved haskell keyword
--
-- >>> escapeHaskellKeyword "test"
-- "test"
--
-- >>> escapeHaskellKeyword "type"
-- "type_"
escapeHaskellKeyword :: Text -> Text
escapeHaskellKeyword name = if toLower name `elem` haskellKeywords then name <> "_" else name
{-# INLINE escapeHaskellKeyword #-}

{-# INLINE haskellKeywords #-}
haskellKeywords :: [Text]
haskellKeywords = [ "_"
    , "as"
    , "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "hiding"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "qualified"
    , "then"
    , "type"
    , "where"
    , "forall"
    , "mdo"
    , "family"
    , "role"
    , "pattern"
    , "static"
    , "group"
    , "by"
    , "using"
    , "foreign"
    , "export"
    , "label"
    , "dynamic"
    , "safe"
    , "interruptible"
    , "unsafe"
    , "stdcall"
    , "ccall"
    , "capi"
    , "prim"
    , "javascript"
    , "rec"
    , "proc"
    ]
