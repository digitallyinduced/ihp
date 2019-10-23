{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType, BlockArguments #-}

module TurboHaskell.Controller.Param where
import           ClassyPrelude
import qualified Data.Either as Either
import           Data.String.Conversions              (cs)
import qualified Data.Text.Read
import           TurboHaskell.Controller.RequestContext
import           TurboHaskell.HaskellSupport
import qualified Network.Wai as Wai
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
import qualified Control.Monad.State.Strict as State
import TurboHaskell.ValidationSupport
import Data.Default
import qualified Data.Dynamic as Dynamic
import GHC.Records
import Control.Monad.State
import qualified TurboHaskell.NameSupport as NameSupport
import TurboHaskell.HaskellSupport
import qualified Data.ByteString.Lazy as LBS
import qualified System.Process as Process

import GHC.TypeLits
import Data.Proxy
import TurboHaskell.ControllerSupport
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.Maybe as Maybe
import qualified System.Process as Process

{-# INLINE fileOrNothing #-}
fileOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe (FileInfo LBS.ByteString)
fileOrNothing !name = ?requestContext |> getField @"files" |> lookup name

{-# INLINE param #-}
param :: (?requestContext :: RequestContext) => (FromParameter a) => ByteString -> a
param !name =
    let
        notFoundMessage = "param: Parameter '" <> cs name <> "' not found"
        parserErrorMessage = "param: Parameter '" <> cs name <> "' is invalid"
    in case paramOrNothing name of
        Just value -> Either.fromRight (error parserErrorMessage) (fromParameter value)
        Nothing -> error notFoundMessage
            

{-# INLINE hasParam #-}
hasParam :: (?requestContext :: RequestContext) => ByteString -> Bool
hasParam = isJust . paramOrNothing'

{-# INLINE paramOrDefault #-}
paramOrDefault :: (?requestContext :: RequestContext) => FromParameter a => a -> ByteString -> a
paramOrDefault !defaultValue = Maybe.fromMaybe defaultValue . paramOrNothing

{-# INLINE paramOrNothing #-}
paramOrNothing :: (?requestContext :: RequestContext) => FromParameter a => ByteString -> Maybe a
paramOrNothing !name = case paramOrNothing' name of
    Just value -> case fromParameter value of
        Left error -> Nothing
        Right value -> Just value
    Nothing -> Nothing

{-# INLINE paramOrNothing' #-}
paramOrNothing' :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
paramOrNothing' !name = do
    let (RequestContext { request, params }) = ?requestContext
    let
        allParams :: [(ByteString, Maybe ByteString)]
        allParams = concat [(map (\(a, b) -> (a, Just b)) params), (Wai.queryString request)]
    join (lookup name allParams)

class ParamName a where
    paramName :: a -> ByteString

instance ParamName ByteString where
    {-# INLINE paramName #-}
    paramName = ClassyPrelude.id

params :: (?requestContext :: RequestContext) => ParamName a => [a] -> [(a, ByteString)]
params = map (\name -> (name, param $ paramName name))


class FromParameter a where
    fromParameter :: ByteString -> Either ByteString a

instance FromParameter ByteString where
    {-# INLINE fromParameter #-}
    fromParameter byteString = pure byteString

instance FromParameter Int where
    {-# INLINE fromParameter #-}
    fromParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left error -> Left ("FromParameter Int: " <> cs error)

instance FromParameter Text where
    {-# INLINE fromParameter #-}
    fromParameter byteString = pure (cs byteString)

instance FromParameter Bool where
    {-# INLINE fromParameter #-}
    fromParameter on | on == cs (ModelSupport.inputValue True) = pure True
    fromParameter _ = pure False

instance FromParameter UUID where
    {-# INLINE fromParameter #-}
    fromParameter byteString =
        case Data.UUID.fromASCIIBytes byteString of
            Just uuid -> pure uuid
            Nothing -> Left "FromParamter UUID: Parse error"

instance FromParameter UTCTime where
    {-# INLINE fromParameter #-}
    fromParameter "" = Left "FromParameter UTCTime: Parameter missing"
    fromParameter byteString =
        let
            input = (cs byteString)
            dateTime = parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
            date = parseTime defaultTimeLocale "%Y-%-m-%d" input
        in case dateTime of
            Nothing -> case date of
                Just value -> Right value
                Nothing -> Left "FromParameter UTCTime: Failed parsing"
            Just value -> Right value

instance FromParameter Point where
    {-# INLINE fromParameter #-}
    fromParameter "" = Left "FromParameter Point: Parameter missing"
    fromParameter byteString =
        let [x, y] = Char8.split ',' byteString
        in 
            case (Data.Text.Read.rational $ cs x) of
                Left error -> Left (cs error)
                Right (x, _) ->
                    case (Data.Text.Read.rational $ cs y) of
                        Left error -> Left (cs error)
                        Right (y, _) -> Right (Point x y)

instance {-# OVERLAPS #-} FromParameter (ModelSupport.Id' model') where
    {-# INLINE fromParameter #-}
    fromParameter uuid =
        case (fromParameter uuid) :: Either ByteString UUID of
            Right uuid -> pure (ModelSupport.Id uuid)
            Left error -> Left error

instance FromParameter param => FromParameter (ModelSupport.FieldWithDefault param) where
    {-# INLINE fromParameter #-}
    fromParameter param = fromParameter param

instance FromParameter param => FromParameter (Maybe param) where
    {-# INLINE fromParameter #-}
    fromParameter param =
        case (fromParameter param) :: Either ByteString param of
            Right value -> Right (Just value)
            Left error -> Left error

instance {-# OVERLAPPABLE #-} (Enum parameter, ModelSupport.InputValue parameter) => FromParameter parameter where
    fromParameter string =
            case find (\value -> ModelSupport.inputValue value == string') allValues of
                Just value -> Right value
                Nothing -> Left "Invalid value"
        where
            string' = cs string
            allValues = enumFrom (toEnum 0) :: [parameter]

class FillParams (params :: [Symbol]) record where
    fill :: (
        ?requestContext :: RequestContext
        , HasField "meta" record ModelSupport.MetaBag
        , SetField "meta" record ModelSupport.MetaBag
        ) => record -> record

instance FillParams ('[]) record where
    fill !record = record

instance (FillParams rest record
    , KnownSymbol fieldName
    , SetField fieldName record fieldType
    , FromParameter fieldType
    , HasField "meta" record ModelSupport.MetaBag
    , SetField "meta" record ModelSupport.MetaBag
    ) => FillParams (fieldName:rest) record where
    fill !record = do
        let name :: ByteString = cs $! NameSupport.fieldNameToColumnName $! cs (symbolVal (Proxy @fieldName))
        case paramOrNothing name of
            Just !paramValue ->
                case fromParameter paramValue of
                    Left !error -> fill @rest (attachFailure (Proxy @fieldName) (cs error) record)
                    Right !(value :: fieldType) -> fill @rest (setField @fieldName value record)
            Nothing -> fill @rest record

ifValid :: (HasField "meta" model ModelSupport.MetaBag) => (Either model model -> IO r) -> model -> IO r
ifValid branch model = branch ((if null annotations then Right else Left) model)
    where
        annotations :: [(Text, Text)]
        annotations = getField @"annotations" meta
        meta :: ModelSupport.MetaBag
        meta = getField @"meta" model

ifNew :: forall record id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, ModelSupport.IsNewId id, GHC.Records.HasField "id" record id, ModelSupport.IsNewId id) => (record -> record) -> record -> record
ifNew thenBlock record = if ModelSupport.isNew record then thenBlock record else record


data ImageUploadOptions = ImageUploadOptions { convertTo :: Text, imageMagickOptions :: Text }



-- TODO: Rename to `uploadPng`
uploadFile :: _ => Proxy fieldName -> record -> IO record
uploadFile field user = uploadImageFile "png" field user

uploadSVG :: _ => Proxy fieldName -> record -> IO record
uploadSVG = uploadImageFile "svg"

uploadImageWithOptions :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?requestContext :: RequestContext
        , ?modelContext :: ModelSupport.ModelContext
        , ?controllerContext :: context
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
    ) => ImageUploadOptions -> Proxy fieldName -> record -> IO record
uploadImageWithOptions options _ user =
    let
        ext = "jpg" :: Text
        fieldName :: ByteString = cs (NameSupport.fieldNameToColumnName (cs (symbolVal (Proxy @fieldName))))
        tableName :: Text = cs (symbolVal (Proxy @tableName))
        uploadDir :: Text = "static"
        baseImagePath :: Text = "/uploads/" <> tableName <> "/" <> tshow (getField @"id" user) <> "/picture."
        imagePath :: Text = baseImagePath <> "jpg"
        uploadFilePath = baseImagePath <> "upload"
    in case fileOrNothing fieldName of
        Just file | fileContent file /= "" -> liftIO do
            _ <- Process.system ("mkdir -p `dirname " <> cs (uploadDir <> uploadFilePath) <> "`")
            let fullImagePath = uploadDir <> imagePath
            (fileContent file) |> LBS.writeFile (cs (uploadDir <> uploadFilePath))
            Process.runCommand (cs ("convert " <> cs uploadDir <> uploadFilePath <> " " <> (getField @"imageMagickOptions" options) <> " " <> cs fullImagePath))
            user
                |> setField @fieldName (Just (cs imagePath :: Text))
                |> return
        _ -> return user

uploadImageFile :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?requestContext :: RequestContext
        , ?modelContext :: ModelSupport.ModelContext
        , ?controllerContext :: context
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
    ) => Text -> Proxy fieldName -> record -> IO record
uploadImageFile ext _ user =
    let
        fieldName :: ByteString = cs (NameSupport.fieldNameToColumnName (cs (symbolVal (Proxy @fieldName))))
        tableName :: Text = cs (symbolVal (Proxy @tableName))
        uploadDir :: Text = "static"
        imagePath :: Text = "/uploads/" <> tableName <> "/" <> tshow (getField @"id" user) <> "/picture." <> ext
    in case fileOrNothing fieldName of
        Just file | fileContent file /= "" -> liftIO do
            _ <- Process.system ("mkdir -p `dirname " <> cs (uploadDir <> imagePath) <> "`")
            (fileContent file) |> LBS.writeFile (cs $ uploadDir <> imagePath)
            user
                |> setField @fieldName (Just (cs imagePath :: Text))
                |> return
        _ -> return user
