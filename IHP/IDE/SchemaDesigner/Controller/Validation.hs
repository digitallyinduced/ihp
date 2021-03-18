module IHP.IDE.SchemaDesigner.Controller.Validation where

import IHP.ControllerPrelude

isUniqueInList :: (Foldable t, Eq a) => t a -> Maybe a -> Validator a
isUniqueInList list oldValue newValue
    | newValue `elem` list && Just newValue /= oldValue = Failure "Value is in forbidden list and not equal to old value"
    | otherwise = Success

isNotIllegalKeyword :: Validator Text
isNotIllegalKeyword name
    | isIllegalKeyword name = Failure $ tshow name <> " is a reserved keyword and can not be used as a name"
    | otherwise = Success

validateNameInSchema :: Text -> [Text] -> Maybe Text -> Validator Text
validateNameInSchema nameType namesInUse oldName =
    validateAll [ nonEmpty |> withCustomErrorMessage (ucfirst nameType <> " cannot be empty")
                , isNotIllegalKeyword
                , isUniqueInList namesInUse oldName
                    |> withCustomErrorMessage (ucfirst nameType <> " is already used")
                ]

isIllegalKeyword :: Text -> Bool
isIllegalKeyword input =
    case input of
        "_" -> True
        _ -> isSQLKeyword input || isHaskellKeyword input

isSQLKeyword :: Text -> Bool
isSQLKeyword input = case (toUpper input) of
    "BIGINT" -> True
    "BIT" -> True
    "BOOLEAN" -> True
    "CHAR" -> True
    "CHARACTER" -> True
    "COALESCE" -> True
    "CONVERT" -> True
    "DEC" -> True
    "DECIMAL" -> True
    "EXISTS" -> True
    "EXTRACT" -> True
    "FLOAT" -> True
    "GREATEST" -> True
    "INOUT" -> True
    "INT" -> True
    "INTEGER" -> True
    "INTERVAL" -> True
    "LEAST" -> True
    "NATIONAL" -> True
    "NCHAR" -> True
    "NONE" -> True
    "NULLIF" -> True
    "NUMERIC" -> True
    "OUT" -> True
    "OVERLAY" -> True
    "POSITION" -> True
    "PRECISION" -> True
    "REAL" -> True
    "ROW" -> True
    "SETOF" -> True
    "SMALLINT" -> True
    "SUBSTRING" -> True
    "TIME" -> True
    "TIMESTAMP" -> True
    "TREAT" -> True
    "TRIM" -> True
    "VARCHAR" -> True
    "ALL" -> True
    "ANALYSE" -> True
    "ANALYZE" -> True
    "AND" -> True
    "ANY" -> True
    "ARRAY" -> True
    "AS" -> True
    "ASC" -> True
    "ASYMMETRIC" -> True
    "BOTH" -> True
    "CASE" -> True
    "CAST" -> True
    "CHECK" -> True
    "COLLATE" -> True
    "COLUMN" -> True
    "CONSTRAINT" -> True
    "CREATE" -> True
    "CURRENT_DATE" -> True
    "CURRENT_ROLE" -> True
    "CURRENT_TIME" -> True
    "CURRENT_TIMESTAMP" -> True
    "CURRENT_USER" -> True
    "DEFAULT" -> True
    "DEFERRABLE" -> True
    "DESC" -> True
    "DISTINCT" -> True
    "DO" -> True
    "ELSE" -> True
    "END" -> True
    "EXCEPT" -> True
    "FALSE" -> True
    "FOR" -> True
    "FOREIGN" -> True
    "FROM" -> True
    "GRANT" -> True
    "GROUP" -> True
    "HAVING" -> True
    "IN" -> True
    "INITIALLY" -> True
    "INTERSECT" -> True
    "INTO" -> True
    "LEADING" -> True
    "LIMIT" -> True
    "LOCALTIME" -> True
    "LOCALTIMESTAMP" -> True
    "NEW" -> True
    "NOT" -> True
    "NULL" -> True
    "OFF" -> True
    "OFFSET" -> True
    "OLD" -> True
    "ON" -> True
    "ONLY" -> True
    "OR" -> True
    "ORDER" -> True
    "PLACING" -> True
    "PRIMARY" -> True
    "REFERENCES" -> True
    "SELECT" -> True
    "SESSION_USER" -> True
    "SOME" -> True
    "SYMMETRIC" -> True
    "TABLE" -> True
    "THEN" -> True
    "TO" -> True
    "TRAILING" -> True
    "TRUE" -> True
    "UNION" -> True
    "UNIQUE" -> True
    "USER" -> True
    "USING" -> True
    "WHEN" -> True
    "WHERE" -> True
    "AUTHORIZATION" -> True
    "BETWEEN" -> True
    "BINARY" -> True
    "CROSS" -> True
    "FREEZE" -> True
    "FULL" -> True
    "ILIKE" -> True
    "INNER" -> True
    "IS" -> True
    "ISNULL" -> True
    "JOIN" -> True
    "LEFT" -> True
    "LIKE" -> True
    "NATURAL" -> True
    "NOTNULL" -> True
    "OUTER" -> True
    "OVERLAPS" -> True
    "RIGHT" -> True
    "SIMILAR" -> True
    "VERBOSE" -> True
    "BYTEA" -> True
    _ -> False

-- "toLower" feels more natural for Haskell keywords
isHaskellKeyword :: Text -> Bool
isHaskellKeyword input =
    case (toLower input) of
        "as" -> True
        "case" -> True
        "class" -> True
        "data" -> True
        "default" -> True
        "deriving" -> True
        "do" -> True
        "else" -> True
        "hiding" -> True
        "if" -> True
        "import" -> True
        "in" -> True
        "infix" -> True
        "infixl" -> True
        "infixr" -> True
        "instance" -> True
        "let" -> True
        "module" -> True
        "newtype" -> True
        "of" -> True
        "qualified" -> True
        "then" -> True
        "type" -> True
        "where" -> True
        "forall" -> True
        "mdo" -> True
        "family" -> True
        "role" -> True
        "pattern" -> True
        "static" -> True
        "group" -> True
        "by" -> True
        "using" -> True
        "foreign" -> True
        "export" -> True
        "label" -> True
        "dynamic" -> True
        "safe" -> True
        "interruptible" -> True
        "unsafe" -> True
        "stdcall" -> True
        "ccall" -> True
        "capi" -> True
        "prim" -> True
        "javascript" -> True
        "rec" -> True
        "proc" -> True
        _ -> False