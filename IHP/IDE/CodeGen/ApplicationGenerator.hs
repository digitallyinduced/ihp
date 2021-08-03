module IHP.IDE.CodeGen.ApplicationGenerator (buildPlan) where

import IHP.Prelude
import IHP.HaskellSupport
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IHP.ViewSupport
import qualified System.Process as Process
import IHP.IDE.CodeGen.Types
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesigner
import IHP.IDE.SchemaDesigner.Types
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

buildPlan :: Text -> IO (Either Text [GeneratorAction])
buildPlan applicationName =
    if (null applicationName)
        then do
            pure $ Left "Application name cannot be empty"
        else do
            let applicationName' = ucfirst applicationName
            pure $ Right $ generateGenericApplication applicationName'

generateGenericApplication :: Text -> [GeneratorAction]
generateGenericApplication applicationName =
        let
            typesHs =
                "module " <> applicationName <> ".Types where\n\n"
                <> "import IHP.Prelude\n"
                <> "import IHP.ModelSupport\n"
                <> "import Generated.Types\n\n"
                <> "data " <> applicationName <> "Application = " <> applicationName <> "Application deriving (Eq, Show)\n\n"
                <> "\n"
                <> "data StaticController = WelcomeAction deriving (Eq, Show, Data)"
                <> "\n"

            routesHs =
                "module " <> applicationName <> ".Routes where\n"
                <> "import IHP.RouterPrelude\n"
                <> "import Generated.Types\n"
                <> "import " <> applicationName <> ".Types\n\n"
                <> "-- Generator Marker\n"
                <> "instance AutoRoute StaticController"
            frontControllerHs =
                "module " <> applicationName <> ".FrontController where\n\n"
                <> "import IHP.RouterPrelude\n"
                <> "import " <> applicationName <> ".Controller.Prelude\n"
                <> "import " <> applicationName <> ".View.Layout (defaultLayout)\n\n"
                <> "-- Controller Imports\n"
                <> "import " <> applicationName <> ".Controller.Static\n\n"
                <> "instance FrontController " <> applicationName <> "Application where\n"
                <> "    controllers = \n"
                <> "        [ startPage WelcomeAction\n"
                <> "        -- Generator Marker\n"
                <> "        ]\n\n"
                <> "instance InitControllerContext " <> applicationName <> "Application where\n"
                <> "    initContext = do\n"
                <> "        setLayout defaultLayout\n"
                <> "        initAutoRefresh\n"
            controllerPreludeHs =
                "module " <> applicationName <> ".Controller.Prelude\n"
                <> "( module " <> applicationName <> ".Types\n"
                <> ", module Application.Helper.Controller\n"
                <> ", module IHP.ControllerPrelude\n"
                <> ", module Generated.Types\n"
                <> ")\n"
                <> "where\n\n"
                <> "import " <> applicationName <> ".Types\n"
                <> "import Application.Helper.Controller\n"
                <> "import IHP.ControllerPrelude\n"
                <> "import Generated.Types\n"

            viewLayoutHs =
                "module " <> applicationName <> ".View.Layout (defaultLayout, Html) where\n"
                <> "\n"
                <> "import IHP.ViewPrelude\n"
                <> "import IHP.Environment\n"
                <> "import qualified Text.Blaze.Html5            as H\n"
                <> "import qualified Text.Blaze.Html5.Attributes as A\n"
                <> "import Generated.Types\n"
                <> "import IHP.Controller.RequestContext\n"
                <> "import " <> applicationName <> ".Types\n"
                <> "import " <> applicationName <> ".Routes\n"
                <> "import Application.Helper.View\n"
                <> "\n"
                <> "defaultLayout :: Html -> Html\n"
                <> "defaultLayout inner = H.docTypeHtml ! A.lang \"en\" $ [hsx|\n"
                <> "<head>\n"
                <> "    {metaTags}\n"
                <> "\n"
                <> "    {stylesheets}\n"
                <> "    {scripts}\n"
                <> "\n"
                <> "    <title>{pageTitleOrDefault \"App\"}</title>\n"
                <> "</head>\n"
                <> "<body>\n"
                <> "    <div class=\"container mt-4\">\n"
                <> "        {renderFlashMessages}\n"
                <> "        {inner}\n"
                <> "    </div>\n"
                <> "</body>\n"
                <> "|]\n"
                <> "\n"
                <> "-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production\n"
                <> "-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed\n"
                <> "-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details\n"
                <> "\n"
                <> "stylesheets :: Html\n"
                <> "stylesheets = [hsx|\n"
                <> "        <link rel=\"stylesheet\" href={assetPath \"/vendor/bootstrap.min.css\"}/>\n"
                <> "        <link rel=\"stylesheet\" href={assetPath \"/vendor/flatpickr.min.css\"}/>\n"
                <> "        <link rel=\"stylesheet\" href={assetPath \"/app.css\"}/>\n"
                <> "    |]\n"
                <> "\n"
                <> "scripts :: Html\n"
                <> "scripts = [hsx|\n"
                <> "        {when isDevelopment devScripts}\n"
                <> "        <script src={assetPath \"/vendor/jquery-3.6.0.slim.min.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/timeago.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/popper.min.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/bootstrap.min.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/flatpickr.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/morphdom-umd.min.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/turbolinks.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/turbolinksInstantClick.js\"}></script>\n"
                <> "        <script src={assetPath \"/vendor/turbolinksMorphdom.js\"}></script>\n"
                <> "        <script src={assetPath \"/helpers.js\"}></script>\n"
                <> "        <script src={assetPath \"/ihp-auto-refresh.js\"}></script>\n"
                <> "    |]\n"
                <> "\n"
                <> "devScripts :: Html\n"
                <> "devScripts = [hsx|\n"
                <> "        <script id=\"livereload-script\" src={assetPath \"/livereload.js\"}></script>\n"
                <> "    |]\n"
                <> "\n"
                <> "metaTags :: Html\n"
                <> "metaTags = [hsx|\n"
                <> "    <meta charset=\"utf-8\"/>\n"
                <> "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\"/>\n"
                <> "    <meta property=\"og:title\" content=\"App\"/>\n"
                <> "    <meta property=\"og:type\" content=\"website\"/>\n"
                <> "    <meta property=\"og:url\" content=\"TODO\"/>\n"
                <> "    <meta property=\"og:description\" content=\"TODO\"/>\n"
                <> "    {autoRefreshMeta}\n"
                <> "|]\n"

            viewPreludeHs =
                "module " <> applicationName <> ".View.Prelude\n"
                <> "( module IHP.ViewPrelude\n"
                <> ", module " <> applicationName <> ".View.Layout\n"
                <> ", module Generated.Types\n"
                <> ", module " <> applicationName <> ".Types\n"
                <> ", module Application.Helper.View\n"
                <> ") where\n"
                <> "\n"
                <> "import IHP.ViewPrelude\n"
                <> "import " <> applicationName <> ".View.Layout\n"
                <> "import Generated.Types\n"
                <> "import " <> applicationName <> ".Types\n"
                <> "import " <> applicationName <> ".Routes ()\n"
                <> "import Application.Helper.View\n"

            welcomeControllerStaticHs =
                "module " <> applicationName <> ".Controller.Static where\n"
                <> "import " <> applicationName  <>".Controller.Prelude\n"
                <> "import " <> applicationName  <>".View.Static.Welcome\n"
                <> "\n"
                <> "instance Controller StaticController where\n"
                <> "    action WelcomeAction = render WelcomeView\n"

            welcomeViewStaticHs =
              "module " <> applicationName <> ".View.Static.Welcome where\n"
             <>"import " <> applicationName <> ".View.Prelude\n"
             <>"\n"
             <>"data WelcomeView = WelcomeView\n"
             <>"\n"
             <>"instance View WelcomeView where\n"
             <>"    html WelcomeView = [hsx|\n"
             <>"         <div style=\"background-color: #657b83; padding-top: 2rem; padding-bottom: 2rem; color:hsla(196, 13%, 96%, 1); border-radius: 4px\">\n"
             <>"              <div style=\"max-width: 800px; margin-left: auto; margin-right: auto\">\n"
             <>"                  <h1 style=\"margin-bottom: 2rem; font-size: 2rem; font-weight: 300; border-bottom: 1px solid white; padding-bottom: 0.25rem; border-color: hsla(196, 13%, 60%, 1)\">\n"
             <>"                      IHP\n"
             <>"                  </h1>\n"
             <>"\n"
             <>"                  <h2 style=\"margin-top: 0; margin-bottom: 0rem; font-weight: 900; font-size: 3rem\">\n"
             <>"                      It's working!\n"
             <>"                  </h2>\n"
             <>"\n"
             <>"                  <p style=\"margin-top: 1rem; font-size: 1.75rem; font-weight: 600; color:hsla(196, 13%, 80%, 1)\">\n"
             <>"                     Your new application is up and running.\n"
             <>"                  </p>\n"
             <>"\n"
             <>"                  <a href=\"https://ihp.digitallyinduced.com/Guide/your-first-project.html\" style=\"margin-top: 2rem; background-color: #268bd2; padding: 1rem; border-radius: 3px; color: hsla(205, 69%, 98%, 1); text-decoration: none; font-weight: bold; display: inline-block; box-shadow: 0 4px 6px hsla(205, 69%, 0%, 0.08);  transition: box-shadow 0.2s; transition: transform 0.2s;\" target=\"_blank\">\n"
             <>"                     Learn the Next Steps in the Documentation\n"
             <>"                  </a>\n"
             <>"              </div>\n"
             <>"         </div>\n"
             <>"\n"
             <>"         <div style=\"max-width: 800px; margin-left: auto; margin-right: auto; margin-top: 4rem\">\n"
             <>"              <img src=\"/ihp-welcome-icon.svg\" alt=\"/ihp-welcome-icon\">\n"
             <>"              <p style=\"color: hsla(196, 13%, 50%, 1); margin-top: 4rem\">\n"
             <>"                 You can modify this start page by making changes to \"./Web/View/Static/Welcome.hs\".\n"
             <>"              </p>\n"
             <>"         </div> \n"
             <>"|]"

        in
            [ EnsureDirectory { directory = applicationName }
            , EnsureDirectory { directory = applicationName <> "/Controller" }
            , EnsureDirectory { directory = applicationName <> "/View" }
            , EnsureDirectory { directory = applicationName <> "/View/Static" }
            , AddImport  { filePath = "Main.hs", fileContent = "import " <> applicationName <> ".FrontController" }
            , AddImport  { filePath = "Main.hs", fileContent = "import " <> applicationName <> ".Types" }
            , AddMountToFrontController { filePath = "Main.hs", applicationName = applicationName }
            , CreateFile { filePath = applicationName <> "/Types.hs", fileContent = typesHs }
            , CreateFile { filePath = applicationName <> "/Routes.hs", fileContent = routesHs }
            , CreateFile { filePath = applicationName <> "/FrontController.hs", fileContent = frontControllerHs }
            , CreateFile { filePath = applicationName <> "/Controller/Prelude.hs", fileContent = controllerPreludeHs }
            , CreateFile { filePath = applicationName <> "/View/Layout.hs", fileContent = viewLayoutHs }
            , CreateFile { filePath = applicationName <> "/View/Prelude.hs", fileContent = viewPreludeHs }
            , CreateFile { filePath = applicationName <> "/Controller/Static.hs", fileContent = welcomeControllerStaticHs }
            , CreateFile { filePath = applicationName <> "/View/Static/Welcome.hs", fileContent = welcomeViewStaticHs }
            ]
