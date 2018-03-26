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

param :: (?requestContext :: RequestContext) => FromParameter a => ByteString -> a
param name = fromParameterOrError (paramOrNothing name)

paramOrDefault :: (?requestContext :: RequestContext) => FromParameter a => ByteString -> a -> a
paramOrDefault name defaultValue =
    case fromParameter (paramOrNothing name) of
        Left _ -> defaultValue
        Right value -> value

paramOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
paramOrNothing name = do
    let (RequestContext request _ bodyParams _ _) = ?requestContext
    let
        allParams :: [(ByteString, Maybe ByteString)]
        allParams = concat [(map (\(a, b) -> (a, Just b)) bodyParams), (queryString request)]
    join (lookup name allParams)

paramInt :: (?requestContext :: RequestContext) => ByteString -> Int
paramInt name = fromParameterOrError (paramOrNothing name)

paramText :: (?requestContext :: RequestContext) => ByteString -> Text
paramText name = fromParameterOrError (paramOrNothing name)

paramBool :: (?requestContext :: RequestContext) => ByteString -> Bool
paramBool name = fromParameterOrError (paramOrNothing name)

fromParameterOrError value = Data.Either.fromRight (error $ "Invalid parameter ") (fromParameter value)

class ParamName a where
    paramName :: a -> ByteString

instance ParamName ByteString where
    paramName = ClassyPrelude.id

params :: (?requestContext :: RequestContext) => ParamName a => [a] -> [(a, ByteString)]
params = map (\name -> (name, param $ paramName name))


class FromParameter a where
    fromParameter :: Maybe ByteString -> Either String a

instance FromParameter ByteString where
    fromParameter (Just byteString) = pure byteString
    fromParameter Nothing = Left "FromParameter ByteString: Parameter missing"

instance FromParameter Int where
    fromParameter (Just byteString) =
        case (Data.Text.Read.decimal $ cs $ byteString) of
            Left error -> Left error
            Right (value, _) -> Right value
    fromParameter Nothing = Left "FromParameter Int: Parameter missing"

instance FromParameter Text where
    fromParameter (Just byteString) = pure $ cs byteString
    fromParameter Nothing = Left "FromParameter Text: Parameter missing"

instance FromParameter Bool where
    fromParameter (Just "on") = pure True
    fromParameter _ = pure False