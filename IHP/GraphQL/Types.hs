module IHP.GraphQL.Types where

import IHP.Prelude

-- https://spec.graphql.org/June2018/#sec-Appendix-Grammar-Summary.Document

newtype Document = Document { definitions :: [Definition] }
    deriving (Eq, Show)

data Definition
    = ExecutableDefinition { operation :: OperationDefinition, fragment :: FragmentDefinition }
    | TypeSystemDefintion
    | TypeSystemExtension
    deriving (Eq, Show)

data OperationDefinition
    = OperationDefinition { selectionSet :: ![Selection] }
    deriving (Eq, Show)

data Selection = Field { alias :: !(Maybe Text), name :: !Text, arguments :: !Arguments, directives :: !Directives, selectionSet :: ![Selection] }
    deriving (Eq, Show)

type Arguments = [(Text, Text)]

type Directives = [Text]

data FragmentDefinition = FragmentDefinition
    deriving (Eq, Show)

