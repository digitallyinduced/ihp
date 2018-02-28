module Foundation.Server (run) where
    import ClassyPrelude
    import qualified Network.Wai.Handler.Warp as Warp
    import Network.Wai
    import Foundation.Router (Router, match)

    import Network.Wai.Middleware.RequestLogger (logStdoutDev)
    import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
    import Network.Wai.Middleware.Static
    import Network.HTTP.Types.Status (status404)

    import Foundation.ModelSupport
    import Foundation.ApplicationContext

    import Database.PostgreSQL.Simple

    import qualified Routes

    defaultPort :: Int
    defaultPort = 8000

    run :: IO ()
    run = do
        conn <- connectPostgreSQL "postgresql://localhost:8001/app"
        Warp.runEnv defaultPort $ applyMiddlewares $ application Routes.match (ApplicationContext $ ModelContext conn)

    applyMiddlewares :: Application -> Application
    applyMiddlewares = logStdoutDev . staticPolicy (addBase "static/") . methodOverridePost

    -- TODO: logger middleware
    application :: Router -> ApplicationContext -> Application
    application router applicationContext request respond = do
        case match request router of
            Just application -> application applicationContext request respond
            Nothing -> respond $ responseLBS status404 [] "Not found!"
