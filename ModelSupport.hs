{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module Foundation.ModelSupport where

import Foundation.HaskellSupport
import ClassyPrelude
import Database.PostgreSQL.Simple (Connection)
import qualified Text.Inflections

data ModelContext = ModelContext Connection

class CanCreate a where
    type Created a :: *
    create :: (?modelContext :: ModelContext) => a -> IO (Created a)

class FormField field where
    type Model field :: *
    formFieldName :: field -> Text
    formFieldLabel :: field -> Text
    formFieldLabel field =
        let
            name = formFieldName field
            (Right parts) = Text.Inflections.parseSnakeCase [] name
        in Text.Inflections.titleize parts
    formFieldValue :: field -> Model field -> Text

class InputValue a where
    inputValue :: a -> Text

instance InputValue Text where
    inputValue text = text

instance InputValue Int where
    inputValue = tshow

newtype Equal a = Equal a
