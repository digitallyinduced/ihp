module Foundation.Server (run) where
    import ClassyPrelude
    import qualified Network.Wai.Handler.Warp as Warp
    import Network.Wai
    import Foundation.Router (Router, match)

    import Network.Wai.Middleware.RequestLogger (logStdoutDev)
    import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
    import Network.Wai.Middleware.Static
    import Network.HTTP.Types.Status (status404)
    import Network.Wai.Session (withSession, Session)
    import Network.Wai.Session.ClientSession (clientsessionStore)
    import Web.ClientSession (getDefaultKey)
    import qualified Data.Vault.Lazy as Vault
    import Data.Default (def)
    import Network.Wai.Session.Map (mapStore_)
    import qualified Web.Cookie
    import qualified Data.Time.Clock

    import Foundation.ModelSupport
    import Foundation.ApplicationContext

    import Database.PostgreSQL.Simple

    import qualified Routes
    import qualified Config

    defaultPort :: Int
    defaultPort = 8000

    run :: IO ()
    run = do
        conn <- connectPostgreSQL Config.postgreSQLUrl
        session <- Vault.newKey
        store <- fmap clientsessionStore getDefaultKey
        let applicationContext = ApplicationContext (ModelContext conn) session
        Warp.runEnv defaultPort $ withSession store "SESSION" (def { Web.Cookie.setCookiePath = Just "/", Web.Cookie.setCookieMaxAge = Just (Data.Time.Clock.secondsToDiffTime 60 * 60 * 24 * 30) }) session $ logStdoutDev $ staticPolicy (addBase "static/") $ methodOverridePost $ application Routes.match applicationContext


    -- TODO: logger middleware
    application :: Router -> ApplicationContext -> Application
    application router applicationContext request respond = do
        case match request router of
            Just application -> application applicationContext request respond
            Nothing -> respond $ responseLBS status404 [] "Not found!"
