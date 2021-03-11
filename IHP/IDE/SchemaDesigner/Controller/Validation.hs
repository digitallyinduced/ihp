module IHP.IDE.SchemaDesigner.Controller.Validation where

import IHP.ControllerPrelude
import IHP.IDE.SchemaDesigner.View.Layout (isIllegalKeyword)

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