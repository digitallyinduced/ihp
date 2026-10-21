module IHP.TypedSql.TableReads (readRelations) where

import qualified Data.Data as Data
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified PostgresqlSyntax as Ast
import Prelude

-- | Relations explicitly named in SQL, including nested queries. CTE names are
-- lexical bindings, not relations on which AutoRefresh can install a trigger.
-- Names are quoted for PostgreSQL's to_regclass, preserving schema and case.
readRelations :: Ast.PreparableStmt -> Set.Set Text
readRelations = collect Set.empty

collect :: Data.Data value => Set.Set Text -> value -> Set.Set Text
collect scope value
    | Just (Ast.SelectNoParens with clause sort limit lock) <- Data.cast value =
        withBindings scope with (childrenOf (Ast.SelectNoParens Nothing clause sort limit lock))
    | Just (Ast.UpdateStmt with target clauses from where_ returning) <- Data.cast value =
        withBindings scope with (\bindings -> collect Set.empty target <> collect bindings (clauses, from, where_, returning))
    | Just (Ast.DeleteStmt with target using where_ returning) <- Data.cast value =
        withBindings scope with (\bindings -> collect Set.empty target <> collect bindings (using, where_, returning))
    | Just (Ast.InsertStmt with target rest conflict returning) <- Data.cast value =
        withBindings scope with (childrenOf (Ast.InsertStmt Nothing target rest conflict returning))
    | Just relation <- Data.cast value = case relation of
        Ast.SimpleRelationExpr name _ -> relationName scope name
        Ast.OnlyRelationExpr name _ -> relationName scope name
    | Just (Ast.InsertTarget name _) <- Data.cast value = relationName Set.empty name
    | otherwise = childrenOf value scope

childrenOf :: Data.Data value => value -> Set.Set Text -> Set.Set Text
childrenOf value scope = Set.unions (Data.gmapQ (collect scope) value)

withBindings :: Set.Set Text -> Maybe Ast.WithClause -> (Set.Set Text -> Set.Set Text) -> Set.Set Text
withBindings scope Nothing body = body scope
withBindings scope (Just (Ast.WithClause recursive ctes)) body =
    let entries = toList ctes
        names = Set.fromList [identifier name | Ast.CommonTableExpr name _ _ _ <- entries]
        allBindings = scope <> names
        definitions _ [] = Set.empty
        definitions current (Ast.CommonTableExpr name _ _ query : rest) =
            collect (if recursive then allBindings else current) query
                <> definitions (Set.insert (identifier name) current) rest
     in definitions scope entries <> body allBindings

relationName :: Set.Set Text -> Ast.QualifiedName -> Set.Set Text
relationName scope (Ast.SimpleQualifiedName name)
    | identifier name `Set.member` scope = Set.empty
    | otherwise = Set.singleton (quoted name)
relationName _ (Ast.IndirectedQualifiedName schema (Ast.Indirection parts)) =
    Set.singleton (Text.intercalate "." (quoted schema : [quoted name | Ast.AttrNameIndirectionEl name <- toList parts]))

identifier :: Ast.Ident -> Text
identifier (Ast.QuotedIdent name) = name
identifier (Ast.UnquotedIdent name) = Text.toLower name

quoted :: Ast.Ident -> Text
quoted name = "\"" <> Text.replace "\"" "\"\"" (identifier name) <> "\""
