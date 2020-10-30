module IHP.IDE.ToolServer.ViewContext where

import IHP.Prelude
import qualified IHP.Controller.Session as Session
import IHP.ControllerSupport  (RequestContext (RequestContext))
import IHP.ControllerSupport
import IHP.HaskellSupport
import IHP.ModelSupport
import qualified IHP.ViewSupport as ViewSupport
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.Types
import IHP.IDE.ToolServer.Helper.Controller
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import qualified Data.Text.IO as IO

instance ViewSupport.CreateViewContext ViewContext where
    type ViewApp ViewContext = ToolServerApplication
    createViewContext = do
        flashMessages <- Session.getAndClearFlashMessages
        webControllers <- findWebControllers
        appNames <- findApplications

        let viewContext = ViewContext {
                requestContext = ?context,
                flashMessages,
                controllerContext = ?controllerContext,
                layout = let ?context = viewContext in toolServerLayout,
                appUrl = "http://localhost:" <> tshow appPort,
                webControllers = webControllers,
                appNames = appNames
            }
        pure viewContext


findWebControllers :: IO [Text]
findWebControllers = do
    directoryFiles <-  listDirectory "Web/Controller"
    let controllerFiles :: [Text] =  filter (\x -> not $ "Prelude" `isInfixOf` x || "Context" `isInfixOf` x)  $ map cs directoryFiles
    pure $ map (Text.replace ".hs" "") controllerFiles

findControllers :: Text -> IO [Text]
findControllers application = do
    directoryFiles <-  listDirectory $ cs $ application <> "/Controller"
    let controllerFiles :: [Text] =  filter (\x -> not $ "Prelude" `isInfixOf` x || "Context" `isInfixOf` x)  $ map cs directoryFiles
    pure $ map (Text.replace ".hs" "") controllerFiles

findApplications :: IO ([Text])
findApplications = do
    mainhs <- IO.readFile "Main.hs"
    let imports = filter (\line -> "import " `isPrefixOf` line && ".FrontController" `isSuffixOf` line) (lines mainhs)
    pure (map removeImport imports)
        where
            removeImport line = Text.replace ".FrontController" "" (Text.replace "import " "" line)
