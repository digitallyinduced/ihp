module IHP.GraphQL.Types where

import IHP.Prelude
import qualified Data.HashMap.Strict as HashMap

-- https://spec.graphql.org/June2018/#sec-Appendix-Grammar-Summary.Document

newtype Document = Document { definitions :: [Definition] }
    deriving (Eq, Show)

data Definition
    = ExecutableDefinition { operation :: OperationDefinition, fragment :: FragmentDefinition }
    | TypeSystemDefintion
    | TypeSystemExtension
    deriving (Eq, Show)

data OperationDefinition
    = OperationDefinition
    { operationType :: !OperationType
    , name :: !(Maybe Text)
    , selectionSet :: ![Selection]
    , variableDefinitions :: ![VariableDefinition]
    } deriving (Eq, Show)

data Selection
    = Field
    { alias :: !(Maybe Text)
    , name :: !Text
    , arguments :: ![Argument]
    , directives :: !Directives
    , selectionSet :: ![Selection]
    } deriving (Eq, Show)

type Directives = [Text]

data FragmentDefinition = FragmentDefinition
    deriving (Eq, Show)

data OperationType
    = Query
    | Mutation
    | Subscription
    deriving (Eq, Show)

data VariableDefinition
    = VariableDefinition
    { variableName :: !Text
    , variableType :: !Text
    } deriving (Eq, Show)

data Argument
    = Argument
    { argumentName :: !Text
    , argumentValue :: !Value
    } deriving (Eq, Show)

-- | http://spec.graphql.org/June2018/#Value
data Value
    = Variable !Text
    | IntValue !Int
    | FloatValue !Double
    | StringValue !Text
    | BooleanValue !Bool
    | NullValue
    | EnumValue
    | ListValue
    | ObjectValue (HashMap.HashMap Text Value)
    deriving (Eq, Show)

newtype Variables
    = Variables [Argument]
    deriving (Eq, Show)