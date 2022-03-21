module IHP.IDE.Graph.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Types

import IHP.IDE.Graph.View.Explore
import IHP.IDE.Graph.View.Schema

import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import qualified IHP.GraphQL.ToText as GraphQL
import qualified IHP.GraphQL.SchemaCompiler as GraphQL
import qualified Database.PostgreSQL.Simple as PG
import IHP.IDE.Data.Controller (connectToAppDb, fetchRowsPage)

import qualified Web.JWT as JWT
import qualified Data.Time.Clock.POSIX as Time
import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as BS

instance Controller GraphController where
    action ExploreAction = do
        SchemaDesigner.parseSchemaSql >>= \case
            Left parserError -> fail (cs parserError)
            Right sqlSchema -> do
                let schema = GraphQL.sqlSchemaToGraphQLSchema sqlSchema
                render ExploreView { .. }

    action SchemaAction = do
        SchemaDesigner.parseSchemaSql >>= \case
            Left parserError -> fail (cs parserError)
            Right sqlSchema -> do
                let schema = GraphQL.sqlSchemaToGraphQLSchema sqlSchema
                render SchemaView { .. }

    action GraphUsersAction = do
        connection <- connectToAppDb
        rows :: [[DynamicField]] <- fetchRowsPage connection "users" 1 50
        
        PG.close connection
        
        renderJson rows

    action GetJWTForUserId { userId } = do
        let lifetime = 60 * 60 * 24 * 3

        createdAt <- getCurrentTime
        expiredAt <- addUTCTime lifetime <$> getCurrentTime
    
        let claimsSet = mempty
                { JWT.iss = (JWT.stringOrURI "https://ihp-dev-identity.digitallyinduced.com/")
                , JWT.sub = JWT.stringOrURI (tshow userId)
                , JWT.iat = JWT.numericDate (Time.utcTimeToPOSIXSeconds createdAt)
                , JWT.exp = JWT.numericDate (Time.utcTimeToPOSIXSeconds expiredAt)
                }
    
        jwtSigner <- initJWTSigner
        let token = JWT.encodeSigned jwtSigner mempty claimsSet
        renderPlain (cs token)

initJWTSigner :: IO JWT.Signer
initJWTSigner = do
    appJwt <- Exception.try @Exception.SomeException (BS.readFile "Application/jwt.key")

    let privateKeyText =
            case appJwt of
                Left _ -> error "Could not find JWT"
                Right result -> result


    privateKeyText
            |> JWT.readRsaSecret
            |> Maybe.fromJust
            |> JWT.RSAPrivateKey
            |> pure
