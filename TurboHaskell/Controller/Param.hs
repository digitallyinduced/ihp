{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType #-}

module TurboHaskell.Controller.Param where
import           ClassyPrelude
import qualified Data.ByteString.Lazy
import qualified Data.Either
import           Data.Maybe                           (fromJust)
import           Data.String.Conversions              (cs)
import qualified Data.Text as Text
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


import GHC.Generics

{-# INLINE fileOrNothing #-}
fileOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe (FileInfo Data.ByteString.Lazy.ByteString)
fileOrNothing name = lookup name files
    where
        (RequestContext _ _ _ files _) = ?requestContext

{-# INLINE param #-}
param :: forall a. (?requestContext :: RequestContext) => (FromParameter a) => ByteString -> Either Text a
param name = fromParameter (paramOrNothing' name)

{-# INLINE paramOrError #-}
paramOrError :: forall a. (?requestContext :: RequestContext) => (FromParameter a) => ByteString -> a
paramOrError name = case param name of
        Left message -> error (cs message)
        Right value -> value

{-# INLINE hasParam #-}
hasParam :: (?requestContext :: RequestContext) => ByteString -> Bool
hasParam = isJust . paramOrNothing'

{-# INLINE paramOrDefault #-}
paramOrDefault :: (?requestContext :: RequestContext) => FromParameter a => a -> ByteString -> a
paramOrDefault defaultValue name =
    case fromParameter (paramOrNothing' name) of
        Left _ -> defaultValue
        Right value -> value

{-# INLINE paramOrNothing #-}
paramOrNothing :: (?requestContext :: RequestContext) => FromParameter a => ByteString -> Either Text (Maybe a)
paramOrNothing name = 
    case paramOrNothing' name of
        Just value ->
            case fromParameter (Just value) of
                Right value -> Right (Just value)
                Left error -> Left error
        Nothing -> Right Nothing

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
fromParameterOrError name value =
    let param = fromParameter value
    in Data.Either.fromRight (error ("fromParameterOrError: Invalid parameter " <> cs name <> " => " <> (let (Data.Either.Left errorMessage) = param in cs errorMessage))) param

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

class FromParameter a where
    fromParameter :: Maybe ByteString -> Either Text a

instance FromParameter ByteString where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) = pure byteString
    fromParameter Nothing = Left "FromParameter ByteString: Parameter missing"

instance FromParameter Int where
    {-# INLINE fromParameter #-}
    fromParameter (Just byteString) =
        case (Data.Text.Read.decimal $ cs byteString) of
            Left error -> Left (Text.pack error)
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
                Left error -> Left (Text.pack error)
                Right (x, _) ->
                    case (Data.Text.Read.rational $ cs y) of
                        Left error -> Left (Text.pack error)
                        Right (y, _) -> Right (Point x y)
    fromParameter Nothing = Left "FromParameter Point: Parameter missing"

instance FromParameter (ModelSupport.Id' model') where
    {-# INLINE fromParameter #-}
    fromParameter maybeUUID =
        case (fromParameter maybeUUID) :: Either Text UUID of
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



