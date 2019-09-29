{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType, BlockArguments #-}

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
import Network.Wai.Parse (FileInfo, fileContent)
import qualified Data.UUID
import Data.UUID (UUID)
import qualified TurboHaskell.ModelSupport as ModelSupport
import TurboHaskell.DatabaseSupport.Point
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import GHC.TypeLits
import Control.Lens ()
import Data.Proxy
import qualified Control.Monad.State.Lazy as State
import TurboHaskell.ValidationSupport
import Data.Default
import qualified Data.Dynamic as Dynamic
import GHC.Records
import Control.Monad.State
import qualified TurboHaskell.NameSupport as NameSupport
import TurboHaskell.HaskellSupport
import qualified Data.ByteString.Lazy as LBS
import qualified System.Process as Process
import qualified Control.Monad.State.Lazy as State
import GHC.TypeLits
import Data.Proxy
import TurboHaskell.ControllerSupport

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
    fromParameter (Just byteString) =
        let
            input = (cs byteString)
            dateTime = parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
            date = parseTime defaultTimeLocale "%Y-%-m-%d" input
        in case dateTime of
            Nothing -> case date of
                Just value -> Right value
                Nothing -> Left "FromParameter UTCTime: Failed parsing"
            Just value -> Right value
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

instance {-# OVERLAPS #-} FromParameter (ModelSupport.Id' model') where
    {-# INLINE fromParameter #-}
    fromParameter maybeUUID =
        case (fromParameter maybeUUID) :: Either String UUID of
            Right uuid -> pure (ModelSupport.Id uuid)
            Left error -> Left error

instance FromParameter param => FromParameter (ModelSupport.FieldWithDefault param) where
    {-# INLINE fromParameter #-}
    fromParameter param | isJust param = fromParameter param
    fromParameter Nothing              = Right ModelSupport.Default

instance FromParameter param => FromParameter (Maybe param) where
    {-# INLINE fromParameter #-}
    fromParameter param | isJust param =
        case (fromParameter param) :: Either String param of
            Right value -> Right (Just value)
            Left error -> Left error
    fromParameter Nothing = Right Nothing

instance {-# OVERLAPPABLE #-} (Enum parameter, ModelSupport.InputValue parameter) => FromParameter parameter where
    fromParameter (Just string) =
            case find (\value -> ModelSupport.inputValue value == string') allValues of
                Just value -> Right value
                Nothing -> Left "Invalid value"
        where
            string' = cs string
            allValues = enumFrom (toEnum 0) :: [parameter]
    fromParameter _ = Left "FromParameter Enum: Parameter missing"

class FillParams (params :: [Symbol]) record where
    fill :: (?requestContext :: RequestContext) => record -> State.StateT [(Text, Text)] IO record

instance FillParams ('[]) record where
    fill record = return record

instance (FillParams rest record
    , KnownSymbol fieldName
    , SetField fieldName record fieldType
    , FromParameter fieldType
    ) => FillParams (fieldName:rest) record where
    fill record = do
        let name :: ByteString = cs $ NameSupport.fieldNameToColumnName $ cs (symbolVal (Proxy @fieldName))
        case paramOrNothing name of
            value@(Just paramValue) ->
                case fromParameter value of
                    Left error -> do
                        attachFailure (Proxy @fieldName) (cs error)
                        fill @rest record
                    Right (value :: fieldType) -> fill @rest (setField @fieldName value record)
            Nothing -> fill @rest record


type RecordReader r = r -> StateT [(Text, Text)] IO r

type ParamPipeline record = forall id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, ?controllerContext :: ControllerContext, GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => record -> StateT [(Text, Text)] IO record

runPipeline :: forall model id. (?requestContext :: RequestContext, ?controllerContext :: ControllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable model, ModelSupport.IsNewId id, GHC.Records.HasField "id" model id) => model -> ParamPipeline model -> IO (Either model model)
runPipeline model pipeline = do
        State.evalStateT inner ([] :: [(Text, Text)])
    where
        inner = do
            model <- pipeline model
            result <- State.get
            fromControllerContext @ValidationResults
                    |> (\(ValidationResults results) -> modifyIORef results (\validations -> (Dynamic.toDyn (model, result)):validations))
            return (if null result then Right model else Left model)

ifNew :: forall record id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, ModelSupport.IsNewId id, GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => (record -> StateT [(Text, Text)] IO record) -> record -> StateT [(Text, Text)] IO record
ifNew thenBlock record = if ModelSupport.isNew record then thenBlock record else return record



uploadFile :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?requestContext :: RequestContext
        , ?modelContext :: ModelSupport.ModelContext
        , ?controllerContext :: context
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
    ) => Proxy fieldName -> record -> State.StateT [(Text, Text)] IO record
uploadFile _ user =
    let
        fieldName :: ByteString = cs (NameSupport.fieldNameToColumnName (cs (symbolVal (Proxy @fieldName))))
        tableName :: Text = cs (symbolVal (Proxy @tableName))
        uploadDir :: Text = "static"
        imagePath :: Text = "/uploads/" <> tableName <> "/" <> tshow (getField @"id" user) <> "/picture.png"
    in case fileOrNothing fieldName of
        Just file | fileContent file /= "" -> liftIO do
            _ <- Process.system ("mkdir -p `dirname " <> cs (uploadDir <> imagePath) <> "`")
            (fileContent file) |> LBS.writeFile (cs $ uploadDir <> imagePath)
            user
                |> setField @fieldName (Just (cs imagePath :: Text))
                |> return
        _ -> return user
