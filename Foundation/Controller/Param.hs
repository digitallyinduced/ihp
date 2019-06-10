{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType #-}

module Foundation.Controller.Param where
import           ClassyPrelude
import qualified Data.ByteString.Lazy
import qualified Data.Either
import           Data.Maybe                           (fromJust)
import           Data.String.Conversions              (cs)
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Read
import           Foundation.Controller.RequestContext
import           Foundation.HaskellSupport
import qualified Network.URI
import           Network.Wai                          (Request, Response, ResponseReceived, queryString, requestBody, responseLBS)
import Network.Wai.Parse (FileInfo)
import qualified Data.UUID
import Data.UUID (UUID)
import qualified Foundation.ModelSupport as ModelSupport
import Foundation.DatabaseSupport.Point
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

import qualified Control.Monad.State.Lazy as State
import GHC.TypeLits
import Control.Lens ()
import qualified Control.Lens as Lens
import qualified Data.Generics.Product as Record
import Data.Proxy
import qualified Control.Monad.State.Lazy as State
import Foundation.ValidationSupport
import Data.Default
import qualified Data.Dynamic as Dynamic
import qualified GHC.Records

{-# INLINE fileOrNothing #-}
fileOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe (FileInfo Data.ByteString.Lazy.ByteString)
fileOrNothing name = lookup name files
    where
        (RequestContext _ _ _ files _) = ?requestContext

{-# INLINE param #-}
param :: (?requestContext :: RequestContext) => (Show a, FromParameter a) => ByteString -> a
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
fromParameterOrError :: (Show a, FromParameter a) => ByteString -> Maybe ByteString -> a
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
        case (Data.Text.Read.decimal $ cs $ byteString) of
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

class FillParams (params :: [Symbol]) record where
    fill :: (?requestContext :: RequestContext) => record -> State.StateT (ValidatorResultFor record) IO record

instance FillParams ('[]) record where
    fill record = return record

instance (FillParams rest record, KnownSymbol fieldName, Record.HasField' fieldName record fieldType, Record.HasField' fieldName (ValidatorResultFor record) ValidatorResult, Generic record, Show fieldType, FromParameter fieldType) => FillParams (fieldName:rest) record where
    fill record = do
        -- record <- State.get
        let name :: ByteString = cs (symbolVal (Proxy @fieldName))
        case (fromParameter (paramOrNothing name)) of
            Left error -> do
                validationState <- State.get
                State.put $ Record.setField @fieldName (Failure (cs error)) validationState
                fill @rest record
            Right (value :: fieldType) -> fill @rest (Record.setField @fieldName value record)



--class FromParams record where
--    fromRequest :: (?requestContext :: RequestContext, ?controllerContext :: controllerContext) => State.StateT record IO record

fromParams :: forall record controllerContext. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, FillParams (ModelSupport.ChangeSet record) record, ModelSupport.Record record, Default (ValidatorResultFor record), Record.HasTypes (ValidatorResultFor record) ValidatorResult, ValidateRecord record controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable record, Typeable (ValidatorResultFor record), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic])) => IO (Either record record)
fromParams = fromParams' (ModelSupport.newRecord @record)

fromParams' :: forall record controllerContext. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, FillParams (ModelSupport.ChangeSet record) record, Default (ValidatorResultFor record), Record.HasTypes (ValidatorResultFor record) ValidatorResult, ValidateRecord record controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable record, Typeable (ValidatorResultFor record), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic])) => record -> IO (Either record record)
fromParams' record = fromRequest record

--instance (FillParams (ModelSupport.ChangeSet model) model, ValidateRecord (ModelSupport.New model) controllerContext) => FromParams model where
fromRequest :: forall model controllerContext. (?requestContext :: RequestContext, ?controllerContext :: controllerContext, FillParams (ModelSupport.ChangeSet model) model, Default (ValidatorResultFor model), Record.HasTypes (ValidatorResultFor model) ValidatorResult, ValidateRecord model controllerContext, ?modelContext :: ModelSupport.ModelContext, Typeable model, Typeable (ValidatorResultFor model), GHC.Records.HasField "validations" controllerContext (IORef [Dynamic.Dynamic])) => model -> IO (Either model model)
fromRequest model = do
        result <- State.evalStateT inner (def :: ValidatorResultFor model)
        return result
    where
        inner = do
            model <- fill @(ModelSupport.ChangeSet model) model
            let ?model = model
            validateRecord @model
            result <- State.get
            let validationsHistory :: IORef [Dynamic.Dynamic] = (GHC.Records.getField @"validations" ?controllerContext)
            modifyIORef validationsHistory (\validations -> (Dynamic.toDyn (model, result)):validations)
            -- return (modelToEither $ fst result)
            -- 
            -- return model
            
            return (modelToEither result)

        modelToEither :: forall model result. (?model :: model, Record.HasTypes result ValidatorResult) => result -> Either model model
        modelToEither result =
            let
                validatorResults = Lens.toListOf (Record.types @ValidatorResult) result
                failures = filter isFailure validatorResults
            in
                case failures of
                    [] -> Right ?model
                    _  -> Left ?model