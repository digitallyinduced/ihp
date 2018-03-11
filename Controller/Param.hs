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

param :: (?requestContext :: RequestContext) => ByteString -> ByteString
param name = fromMaybe (error $ "Required parameter " <> cs name <> " is missing") (paramOrNothing name)

paramOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
paramOrNothing name = do
    let (RequestContext request _ bodyParams _ _) = ?requestContext
    let
        allParams :: [(ByteString, Maybe ByteString)]
        allParams = concat [(map (\(a, b) -> (a, Just b)) bodyParams), (queryString request)]
    join (lookup name allParams)

paramInt :: (?requestContext :: RequestContext) => ByteString -> Int
paramInt name = fst $ Data.Either.fromRight (error $ "Invalid parameter " <> cs name) (Data.Text.Read.decimal $ cs $ param name)

paramText :: (?requestContext :: RequestContext) => ByteString -> Text
paramText name = cs $ param name

paramBool :: (?requestContext :: RequestContext) => ByteString -> Bool
paramBool name = case paramOrNothing name of Just "on" -> True; otherwise -> False

class ParamName a where
    paramName :: a -> ByteString

instance ParamName ByteString where
    paramName = ClassyPrelude.id

params :: (?requestContext :: RequestContext) => ParamName a => [a] -> [(a, ByteString)]
params = map (\name -> (name, param $ paramName name))
