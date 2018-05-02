module Foundation.UrlGeneratorCompiler where

import           ClassyPrelude           (when, tshow)
import           Data.Maybe              (fromJust, isJust, catMaybes)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (cs)
import           Data.Text               (Text, intercalate, stripPrefix, toTitle)
import qualified Data.Text
import qualified Foundation.NameSupport
import           Foundation.Router
import           Prelude
import qualified Routes
import qualified System.Directory        as Directory
import qualified Data.Set
import qualified Data.Text as Text
import qualified Model.Schema
import qualified Foundation.SchemaSupport
c = compile
main = c

compile :: IO ()
compile = do
    writeCompiledUrlGenerator (doCompile Routes.match)

doCompile :: Router -> Text
doCompile router =
    let
        namePathPairs = map (\(UrlGenerator path) -> let path' = simplify path in (generateName path', path')) (urlGenerators router (UrlGenerator { path = [] }))
    in
        "-- This file is auto generated and will be overriden regulary. Please edit `src/Routes.hs`\n"
        <> "module UrlGenerator where\n\n"
        <> "import ClassyPrelude\n"
        <> "import Foundation.UrlGeneratorSupport\n"
        <> "import Model.Generated.Types\n"
        <> "\n\n"
        <> (intercalate "\n\n" $ mkUniq $ map generateUrlGeneratorCode namePathPairs)
        <> "\n\n"
        <> (intercalate "\n\n" $ mkUniq $ catMaybes $ map generatePathToCode namePathPairs)


writeCompiledUrlGenerator :: Text -> IO ()
writeCompiledUrlGenerator content = do
    let path = "src/UrlGenerator.hs"
    alreadyExists <- Directory.doesFileExist path
    putStrLn $ "Updating " <> cs path
    writeFile (cs path) (cs content)


generateUrlGeneratorCode (Just name, path) = typeDefinition <> "\n" <> implementation
    where
        typeDefinition = (lcfirst name) <> "Path :: " <> typeConstraints <> intercalate " -> " (map compilePathToType (zip (variablesOnly path) [0..]) <> ["Text"])
        implementation = (lcfirst name) <> "Path " <> compileArgs <> " = " <> intercalate " <> " (map compilePath (zip path [0..]))
        typeConstraints =
            if length (variablesOnly path) > 0
            then "(" <> (intercalate ", " $ map compilePathToTypeConstraint (zip (variablesOnly path) [0..])) <> ") => "
            else ""
        compilePath :: (UrlGeneratorPath, Int) -> Text
        compilePath (Constant value, i) = cs $ "\"/" <> value <> "\""
        compilePath (Variable x, i)     = cs $ "\"/\" <> toText arg" <> tshow i
        compileArgs = intercalate " " $ map fromJust $ filter isJust $ map compileArg $ zip path [0..]
            where
                compileArg :: (UrlGeneratorPath, Int) -> Maybe Text
                compileArg (Variable _, i) = Just $ cs $ "arg" <> tshow i
                compileArg _               = Nothing
        compilePathToType :: (UrlGeneratorPath, Int) -> Text
        compilePathToType (Variable x, i) = "urlArgument" <> tshow i
        compilePathToTypeConstraint :: (UrlGeneratorPath, Int) -> Text
        compilePathToTypeConstraint (Variable x, i) = "UrlArgument urlArgument" <> tshow i
generateUrlGeneratorCode (Nothing, []) = ""
generateUrlGeneratorCode (Nothing, path) = "-- " <> (cs $ tshow path)

generatePathToCode (Just name, path) | "new" `Text.isPrefixOf` name =
        if belongsToModel
            then Just $
                "instance PathTo " <> newModelTypeName <> " where pathTo _ = " <> name <> "Path\n"
            else Nothing
    where
        newModelTypeName = Foundation.NameSupport.ucfirst name
        modelTypeName = fromJust (Text.stripPrefix "new" name)
        belongsToModel = modelTypeName `elem` modelNames
        modelNames = map (\(Foundation.SchemaSupport.Table tableName _) -> Foundation.NameSupport.tableNameToModelName tableName) Model.Schema.database
generatePathToCode (Just name, path) = Just $ "-- " <> name
generatePathToCode (Nothing, path) = Just $ "-- " <> tshow path

generateName = generateNewEditName

generateNewEditName :: [UrlGeneratorPath] -> Maybe Text
generateNewEditName [] = Nothing
generateNewEditName path =
    case last path of
        Constant value ->
            if value == "new" || value == "edit" then
                Just (cs value <> (intercalate "" (map Foundation.NameSupport.pluralToSingular $ unwrappedConstants $ init path)))
            else
                Just (intercalate "" (unwrappedConstants $ map uppercaseFirstLetter path))
        Variable name -> Just (intercalate "" (map Foundation.NameSupport.pluralToSingular $ unwrappedConstants $ path))

unwrappedConstants = map (\(Constant value) -> cs value) . constantsOnly

constantsOnly list = filter isConstant list
    where
        isConstant (Constant _) = True
        isConstant _            = False

variablesOnly list = filter isVariable list
    where
        isVariable (Variable _) = True
        isVariable _            = False

uppercaseFirstLetter (Constant value) = Constant (cs $ Foundation.NameSupport.ucfirst $ cs value)
uppercaseFirstLetter otherwise        = otherwise

simplify :: [UrlGeneratorPath] -> [UrlGeneratorPath]
simplify ((Constant "/"):rest) = simplify rest
simplify ((Constant value):rest) =
    case stripPrefix "/" (cs value) of
        Just stripped -> (Constant (cs stripped)):(simplify rest)
        Nothing       -> (Constant value):(simplify rest)
simplify (x:xs) = x:(simplify xs)
simplify rest = rest

mkUniq :: Ord a => [a] -> [a]
mkUniq = Data.Set.toList . Data.Set.fromList
