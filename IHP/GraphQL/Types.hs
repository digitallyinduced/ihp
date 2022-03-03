module IHP.GraphQL.Types where

import IHP.Prelude
import qualified Data.HashMap.Strict as HashMap

data GraphQLRequest = GraphQLRequest
    { query :: !Document
    , variables :: !Variables
    }

-- https://spec.graphql.org/June2018/#sec-Appendix-Grammar-Summary.Document

newtype Document = Document { definitions :: [Definition] }
    deriving (Eq, Show)

data Definition
    = ExecutableDefinition { operation :: !OperationDefinition }
    | TypeSystemDefinition { typeSystemDefinition :: !TypeSystemDefinition }
    | TypeSystemExtension
    | FragmentDefinition !Fragment
    deriving (Eq, Show)

data TypeSystemDefinition
    = SchemaDefinition
        { queryType :: !Type
        , mutationType :: !Type
        }
    | TypeDefinition !TypeDefinition
    | DirectiveDefinition
    deriving (Eq, Show)

data TypeDefinition
    = ScalarTypeDefinition { name :: !Text }
    | ObjectTypeDefinition { name :: !Text, implementsInterfaces :: [Type], fieldDefinitions :: ![FieldDefinition] }
    | InterfaceTypeDefinition
    | UnionTypeDefinition
    | EnumTypeDefinition
    | InputObjectTypeDefinition
    deriving (Eq, Show)

data OperationDefinition
    = OperationDefinition
    { operationType :: !OperationType
    , name :: !(Maybe Text)
    , selectionSet :: ![Selection]
    , variableDefinitions :: ![VariableDefinition]
    } deriving (Eq, Show)

data FieldDefinition
    = FieldDefinition
    { description :: !(Maybe Text)
    , name :: !Text
    , argumentsDefinition :: Maybe ArgumentsDefinition
    , type_ :: Type
    } deriving (Eq, Show)

data ArgumentsDefinition = ArgumentsDefinition
    deriving (Eq, Show)

data Selection
    = Field
        { alias :: !(Maybe Text)
        , name :: !Text
        , arguments :: ![Argument]
        , directives :: !Directives
        , selectionSet :: ![Selection]
        }
    | FragmentSpread
        { fragmentName :: !Text }
    deriving (Eq, Show)

type Directives = [Text]

data Fragment
    = Fragment
    { name :: Text
    , selectionSet :: ![Selection]
    }
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

data Type
    = NamedType !Text
    | ListType !Type
    | NonNullType !Type
    deriving (Eq, Show)