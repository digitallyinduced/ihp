{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType #-}

module TurboHaskell.Controller.Param where
import           ClassyPrelude
import qualified Data.ByteString.Lazy
import qualified Data.Either
import           Data.Maybe                           (fromJust)
import           Data.String.Conversions              (cs)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Read
import           TurboHaskell.Controller.RequestContext
import           TurboHaskell.HaskellSupport
import qualified Network.URI
import           Network.Wai                          (Request, Response, ResponseReceived, queryString, requestBody, responseLBS)
import Network.Wai.Parse (FileInfo)
import qualified Data.UUID
import Data.UUID (UUID)
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.DatabaseSupport.Point
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import GHC.TypeLits
import Control.Lens ()
import qualified Control.Lens as Lens
import qualified Data.Generics.Product as Record
import Data.Proxy
import qualified Control.Monad.State.Lazy as State
import TurboHaskell.ValidationSupport
import Data.Default
import qualified Data.Dynamic as Dynamic
import qualified GHC.Records
import Control.Monad.State
import qualified TurboHaskell.NameSupport as NameSupport

{-# INLINE fileOrNothing #-}
fileOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe (FileInfo Data.ByteString.Lazy.ByteString)
fileOrNothing name = lookup name files
    where
        (RequestContext _ _ _ files _) = ?requestContext

{-# INLINE param #-}
param :: (?requestContext :: RequestContext) => (FromParameter a) => ByteString -> a
param name = (fromParameterOrError name) (paramOrNothing name)

{-# INLINE hasParam #-}
hasParam :: (?requestContext :: RequestContext) => ByteString -> Bool
hasParam = isJust . paramOrNothing'

{-# INLINE paramOrDefault #-}
paramOrDefault :: (?requestContext :: RequestContext) => FromParameter a => a -> ByteString -> a
paramOrDefault defaultValue name =
    case fromParameter (paramOrNothing name) of
        Left _ -> defaultValue
        Right value -> value

{-# INLINE paramOrNothing #-}
paramOrNothing :: (?requestContext :: RequestContext) => FromParameter a => ByteString -> Maybe a
paramOrNothing name = fromParameterOrNothing name (paramOrNothing' name)

{-# INLINE paramOrNothing' #-}
paramOrNothing' :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
paramOrNothing' name = do
    let (RequestContext request _ bodyParams _ _) = ?requestContext
    let
        allParams :: [(ByteString, Maybe ByteString)]
        allParams = concat [(map (\(a, b) -> (a, Just b)) bodyParams), (queryString request)]
    join (lookup name allParams)

{-# INLINE fromParameterOrError #-}
fromParameterOrError :: (FromParameter a) => ByteString -> Maybe ByteString -> a
fromParameterOrError name value = let param = fromParameter value in Data.Either.fromRight (error $ "fromParameterOrError: Invalid parameter " <> cs name <> " => " <> (let (Data.Either.Left errorMessage) = param in errorMessage)) param

{-# INLINE fromParameterOrNothing #-}
fromParameterOrNothing :: FromParameter a => ByteString -> Maybe ByteString -> Maybe a
fromParameterOrNothing name value =
    case fromParameter value of
        Left _ -> Nothing
        Right value -> Just value

class ParamName a where
    paramName :: a -> ByteString

instance ParamName ByteString where
    {-# INLINE paramName #-}
    paramName = ClassyPrelude.id

params :: (?requestContext :: RequestContext) => ParamName a => [a] -> [(a, ByteString)]
params = map (\name -> (name, param $ paramName name))


class FromParameter a where
    fromParameter :: Maybe ByteString -> Either String a

instance FromParameter ByteString where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) = pure byteString
    fromParameter Nothing = Left "FromParameter ByteString: Parameter missing"

instance FromParameter Int where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) =
        case (Data.Text.Read.decimal $ cs byteString) of
            Left error -> Left error
            Right (value, _) -> Right value
    fromParameter Nothing = Left "FromParameter Int: Parameter missing"

instance FromParameter Text where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) = pure $ cs byteString
    fromParameter Nothing = Left "FromParameter Text: Parameter missing"

instance FromParameter Bool where
    {-# INLINE fromParameter #-}
    fromParameter (Just on) | on == cs (ModelSupport.inputValue True) = pure True
    fromParameter _ = pure False

instance FromParameter UUID where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) =
        case Data.UUID.fromASCIIBytes (byteString) of
            Just uuid -> pure uuid
            Nothing -> Left "FromParamter UUID: Parse error"
    fromParameter Nothing = Left "FromParameter UUID: Parameter missing"

instance FromParameter UTCTime where
    {-# INLINE fromParameter #-}
    fromParameter (Just "") = Left "FromParameter UTCTime: Parameter missing"
    fromParameter (Just byteString) = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (cs byteString)
    fromParameter Nothing = Left "FromParameter UTCTime: Parameter missing"

instance FromParameter Point where
    {-# INLINE fromParameter #-}
    fromParameter (Just "") = Left "FromParameter Point: Parameter missing"
    fromParameter (Just byteString) =
        let [x, y] = Char8.split ',' byteString
        in 
            case (Data.Text.Read.rational $ cs x) of
                Left error -> Left error
                Right (x, _) ->
                    case (Data.Text.Read.rational $ cs y) of
                        Left error -> Left error
                        Right (y, _) -> Right (Point x y)
    fromParameter Nothing = Left "FromParameter Point: Parameter missing"

instance FromParameter (ModelSupport.Id' model') where
    {-# INLINE fromParameter #-}
    fromParameter maybeUUID =
        case (fromParameter maybeUUID) :: Either String UUID of
            Right uuid -> pure (ModelSupport.Id uuid)
            Left error -> Left error

instance FromParameter param => FromParameter (ModelSupport.FieldWithDefault param) where
    {-# INLINE fromParameter #-}
    fromParameter param | isJust param = fromParameter param
    fromParameter Nothing              = Right ModelSupport.Default

instance (Enum parameter, ModelSupport.InputValue parameter) => FromParameter parameter where
    fromParameter (Just string) =
            case find (\value -> ModelSupport.inputValue value == string') allValues of
                Just value -> Right value
                Nothing -> Left "Invalid value"
        where
            string' = cs string
            allValues = enumFrom (toEnum 0) :: [parameter]
    fromParameter _ = Left "FromParameter Enum: Parameter missing"

class FillParams (params :: [Symbol]) record where
    fill :: (?requestContext :: RequestContext) => record -> State.StateT (ValidatorResultFor record) IO record

instance FillParams ('[]) record where
    fill record = return record

instance (FillParams rest record
    , KnownSymbol fieldName
    , Record.HasField' fieldName record fieldType
    , Record.HasField fieldName (ValidatorResultFor record) (ValidatorResultFor record) ValidatorResult ValidatorResult
    , Generic record
    , FromParameter fieldType
    ) => FillParams (fieldName:rest) record where
    fill record = do
        -- record <- State.get
        let name :: ByteString = cs $ NameSupport.fieldNameToColumnName $ cs (symbolVal (Proxy @fieldName))
        case paramOrNothing name of
            value@(Just paramValue) ->
                case fromParameter value of
                    Left error -> do
                        attachFailure (Proxy @fieldName) (cs error)
                        fill @rest record
                    Right (value :: fieldType) -> fill @rest (Record.setField @fieldName value record)
            Nothing -> fill @rest record



fromParams :: forall record controllerContext id. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, ModelSupport.Record record, Default (ValidatorResultFor record), Record.HasTypes (ValidatorResultFor record) ValidatorResult, FromParams record controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable record, Typeable (ValidatorResultFor record), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic]), GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => IO (Either record record)
fromParams = fromParams' (ModelSupport.newRecord @record)

fromParams' :: forall record controllerContext id. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, Default (ValidatorResultFor record), Record.HasTypes (ValidatorResultFor record) ValidatorResult, FromParams record controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable record, Typeable (ValidatorResultFor record), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic]), GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => record -> IO (Either record record)
fromParams' record = fromRequest record

type ParamPipeline record context = forall id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, ?controllerContext :: context, GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => record -> StateT (ValidatorResultFor record) IO record
class FromParams record context where
    build :: ParamPipeline record context

fromRequest :: forall model controllerContext id. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, Default (ValidatorResultFor model), Record.HasTypes (ValidatorResultFor model) ValidatorResult, FromParams model controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable model, Typeable (ValidatorResultFor model), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic]), ModelSupport.IsNewId id, GHC.Records.HasField "id" model id) => model -> IO (Either model model)
fromRequest model = runPipeline model build

runPipeline :: forall model controllerContext id. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, Default (ValidatorResultFor model), Record.HasTypes (ValidatorResultFor model) ValidatorResult, ?modelContext :: ModelSupport.ModelContext, Typeable model, Typeable (ValidatorResultFor model), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic]), ModelSupport.IsNewId id, GHC.Records.HasField "id" model id) => model -> ParamPipeline model controllerContext -> IO (Either model model)
runPipeline model pipeline = do
        State.evalStateT inner (def :: ValidatorResultFor model)
    where
        inner = do
            model <- pipeline model
            result <- State.get
            let validationsHistory :: IORef [Dynamic.Dynamic] = (GHC.Records.getField @"validations" ?controllerContext)
            modifyIORef validationsHistory (\validations -> (Dynamic.toDyn (model, result)):validations)
            return (modelToEither model result)

        modelToEither :: forall model result. (Record.HasTypes result ValidatorResult) => model -> result -> Either model model
        modelToEither model result =
            let
                validatorResults = Lens.toListOf (Record.types @ValidatorResult) result
                failures = filter isFailure validatorResults
            in
                case failures of
                    [] -> Right model
                    _  -> Left model

ifNew :: forall record id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, ModelSupport.IsNewId id, GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => (record -> StateT (ValidatorResultFor record) IO record) -> record -> StateT (ValidatorResultFor record) IO record
ifNew thenBlock record = if ModelSupport.isNew record then thenBlock record else return record