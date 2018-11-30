{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances #-}

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
    fromParameter (Just byteString) = parseTimeM True defaultTimeLocale "%Y-%-m-%-d %R" (cs byteString)
    fromParameter Nothing = Left "FromParameter UTCTime: Parameter missing"

instance {-# OVERLAPPABLE #-} (Show idField, ModelSupport.NewTypeWrappedUUID idField) => FromParameter idField where
    {-# INLINE fromParameter #-}
    fromParameter maybeUUID =
        case (fromParameter maybeUUID) :: Either String UUID of
            Right uuid -> pure (ModelSupport.wrap uuid)
            Left error -> Left error
