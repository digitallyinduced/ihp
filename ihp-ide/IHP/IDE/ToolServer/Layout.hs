module IHP.IDE.ToolServer.Layout where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes ()
import qualified IHP.Version as Version
import IHP.IDE.ToolServer.Helper.View

toolServerLayout :: Html -> Html
toolServerLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"/>

        <link rel="shortcut icon" type="image/x-icon" href="/ihp-icon-white-bg.svg"/>
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/IDE/schema-designer.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/select2.min.css"}/>

        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/jquery-3.6.0.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        

        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        

        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/IDE/contextmenu.js"}></script>

        <script src={assetPath "/vendor/select2.min.js"}></script>
        <script src={assetPath "/vendor/src-min/ace.js"}></script>
        <script src={assetPath "/vendor/src-min/ext-language_tools.js"}></script>
        <script src={assetPath "/IDE/ihp-schemadesigner.js"}></script>
        <script src={assetPath "/IDE/ihp-codegen.js"}></script>
        <script src={assetPath "/IDE/ihp-policy.js"}></script>
        <script src={assetPath "/IDE/ihp-help.js"}></script>
        <script src={assetPath "/IDE/query-editor.js"}></script>
        <script src={assetPath "/IDE/data-hovercard.js"}></script>
        <script src={assetPath "/IDE/migration-editor.js"}></script>


        <title>IHP IDE</title>
    </head>
    <body class="d-flex h-100 flex-row">
        <div id="nav">
            <img id="nav-logo" src="/ihp-icon.svg" alt="IHP: Integrated Haskell Platform">
            <div id="ihp-plan">{ihpEditionTitle}</div>
            {apps}
            {schema}
            {data_}
            {codegen}
            {logs}
            {docu}

            {when isBasicEdition getPro}
            {help}
            <a href="https://www.digitallyinduced.com/" id="nav-copyright" target="_blank">©<br />digitally induced GmbH</a>
        </div>
        <div id="content">
            {inner}
        </div>
    </body>
</html>
|]  where
        (AvailableApps appNames) = fromFrozenContext @AvailableApps
        apps = forEach appNames appNavItem
        schema = navItem "SCHEMA" schemaIcon (pathTo TablesAction) (isSchemaEditorController)
        data_ = navItem "DATA" dataIcon (pathTo ShowDatabaseAction) (isActiveController @DataController)
        repl = navItem "REPL" terminalIcon "#" False
        codegen = navItem "CODEGEN" copyIcon (pathTo GeneratorsAction) (isActiveController @CodeGenController)
        logs = navItem "LOGS" serverIcon (pathTo AppLogsAction) (isActiveController @LogsController)
        lint = navItem "LINT" flagIcon "#" False
        docu = navItem "DOCS" docsIcon "https://ihp.digitallyinduced.com/Guide/" False
        
        isSchemaEditorController =
                    (  isActiveController @SchemaController
                    || isActiveController @TablesController
                    || isActiveController @ColumnsController
                    || isActiveController @EnumsController
                    || isActiveController @EnumValuesController
                    || isActiveController @MigrationsController )

        help :: Html
        help = [hsx|
            <a
                href="#"
                class="nav-item"
                data-container="body"
                data-toggle="popover"
                data-placement="right"
                data-title="Questions, or need help with Haskell type errors?"
                data-trigger="focus"
                id="nav-help"
            >
                {helpIcon}
            </a>
            <div id="help-content" style="display: none">
                <a class="btn btn-light border btn-block mb-1" href="https://stackoverflow.com/questions/tagged/ihp" target="_blank">
                    → StackOverflow
                </a>

                <p class="text-muted text-center">
                    <small>
                        IHP Version: {Version.ihpVersion}
                    </small>
                </p>

                <p class="text-muted email-support">
                    If you're using <a href="https://ihp.digitallyinduced.com/Pricing" target="_blank">IHP Business</a>, you can also <a href="mailto:support@digitallyinduced.com">reach out to the digitally induced email support</a>.
                </p>
            </div>
        |]

        getPro :: Html
        getPro = [hsx|
            <a
                href="https://ihp.digitallyinduced.com/Pricing?source=ide"
                class="nav-item text-center"
                target="_blank"
                id="nav-upgrade"
            >
                Upgrade to <br /> IHP Pro
            </a>
        |]

        ihpEditionTitle = case Version.ihpEdition of
            Version.Basic -> [hsx|IHP|]
            Version.Pro -> [hsx|IHP Pro|]
            Version.Business -> [hsx|IHP <br />Business|]
            Version.Enterprise -> [hsx|IHP <br />Enterprise|]

        appNavItem :: Text -> Html
        appNavItem "Web" = navItem "APP" startIcon (appUrl <> "/") False
        appNavItem name = navItem (toUpper name) startIcon (appUrl <> "/" <> (toLower name) <> "/") False

        navItem :: Text -> Html -> Text -> Bool -> Html
        navItem label icon action active = [hsx|
        <a href={action} class={classes [("nav-item", True), ("active", active)]} target={target}>
            {icon}
            {label}
        </a>
        |]
            where
                isExternal = "https://" `isPrefixOf` action || "http://" `isPrefixOf` action
                target :: Maybe Text
                target = if isExternal then "_blank" else Nothing

appUrl :: (?context :: ControllerContext) => Text
appUrl = let (AppUrl url) = fromFrozenContext @AppUrl in url

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/terminal.svg
terminalIcon = preEscapedToHtml [plain|<svg viewBox="0 0 1792 1792" xmlns="http://www.w3.org/2000/svg"><path d="M649 983l-466 466q-10 10-23 10t-23-10l-50-50q-10-10-10-23t10-23l393-393-393-393q-10-10-10-23t10-23l50-50q10-10 23-10t23 10l466 466q10 10 10 23t-10 23zm1079 457v64q0 14-9 23t-23 9h-960q-14 0-23-9t-9-23v-64q0-14 9-23t23-9h960q14 0 23 9t9 23z" fill="#fff"/></svg>|]

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/copy.svg
copyIcon = preEscapedToHtml [plain|<svg viewBox="0 0 1792 1792" xmlns="http://www.w3.org/2000/svg"><path d="M1696 384q40 0 68 28t28 68v1216q0 40-28 68t-68 28h-960q-40 0-68-28t-28-68v-288h-544q-40 0-68-28t-28-68v-672q0-40 20-88t48-76l408-408q28-28 76-48t88-20h416q40 0 68 28t28 68v328q68-40 128-40h416zm-544 213l-299 299h299v-299zm-640-384l-299 299h299v-299zm196 647l316-316v-416h-384v416q0 40-28 68t-68 28h-416v640h512v-256q0-40 20-88t48-76zm956 804v-1152h-384v416q0 40-28 68t-68 28h-416v640h896z" fill="currentColor"/></svg>|]

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/server.svg
serverIcon = preEscapedToHtml [plain|<svg viewBox="0 0 1792 1792" xmlns="http://www.w3.org/2000/svg"><path d="M128 1408h1024v-128h-1024v128zm0-512h1024v-128h-1024v128zm1568 448q0-40-28-68t-68-28-68 28-28 68 28 68 68 28 68-28 28-68zm-1568-960h1024v-128h-1024v128zm1568 448q0-40-28-68t-68-28-68 28-28 68 28 68 68 28 68-28 28-68zm0-512q0-40-28-68t-68-28-68 28-28 68 28 68 68 28 68-28 28-68zm96 832v384h-1792v-384h1792zm0-512v384h-1792v-384h1792zm0-512v384h-1792v-384h1792z" fill="#fff"/></svg>|]

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/flag.svg
flagIcon = preEscapedToHtml [plain|<svg viewBox="0 0 1792 1792" xmlns="http://www.w3.org/2000/svg"><path d="M320 256q0 72-64 110v1266q0 13-9.5 22.5t-22.5 9.5h-64q-13 0-22.5-9.5t-9.5-22.5v-1266q-64-38-64-110 0-53 37.5-90.5t90.5-37.5 90.5 37.5 37.5 90.5zm1472 64v763q0 25-12.5 38.5t-39.5 27.5q-215 116-369 116-61 0-123.5-22t-108.5-48-115.5-48-142.5-22q-192 0-464 146-17 9-33 9-26 0-45-19t-19-45v-742q0-32 31-55 21-14 79-43 236-120 421-120 107 0 200 29t219 88q38 19 88 19 54 0 117.5-21t110-47 88-47 54.5-21q26 0 45 19t19 45z" fill="#fff"/></svg>|]

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/cogs.svg
cogsIcon = preEscapedToHtml [plain|<svg viewBox="0 0 2048 1792" xmlns="http://www.w3.org/2000/svg"><path d="M960 896q0-106-75-181t-181-75-181 75-75 181 75 181 181 75 181-75 75-181zm768 512q0-52-38-90t-90-38-90 38-38 90q0 53 37.5 90.5t90.5 37.5 90.5-37.5 37.5-90.5zm0-1024q0-52-38-90t-90-38-90 38-38 90q0 53 37.5 90.5t90.5 37.5 90.5-37.5 37.5-90.5zm-384 421v185q0 10-7 19.5t-16 10.5l-155 24q-11 35-32 76 34 48 90 115 7 11 7 20 0 12-7 19-23 30-82.5 89.5t-78.5 59.5q-11 0-21-7l-115-90q-37 19-77 31-11 108-23 155-7 24-30 24h-186q-11 0-20-7.5t-10-17.5l-23-153q-34-10-75-31l-118 89q-7 7-20 7-11 0-21-8-144-133-144-160 0-9 7-19 10-14 41-53t47-61q-23-44-35-82l-152-24q-10-1-17-9.5t-7-19.5v-185q0-10 7-19.5t16-10.5l155-24q11-35 32-76-34-48-90-115-7-11-7-20 0-12 7-20 22-30 82-89t79-59q11 0 21 7l115 90q34-18 77-32 11-108 23-154 7-24 30-24h186q11 0 20 7.5t10 17.5l23 153q34 10 75 31l118-89q8-7 20-7 11 0 21 8 144 133 144 160 0 8-7 19-12 16-42 54t-45 60q23 48 34 82l152 23q10 2 17 10.5t7 19.5zm640 533v140q0 16-149 31-12 27-30 52 51 113 51 138 0 4-4 7-122 71-124 71-8 0-46-47t-52-68q-20 2-30 2t-30-2q-14 21-52 68t-46 47q-2 0-124-71-4-3-4-7 0-25 51-138-18-25-30-52-149-15-149-31v-140q0-16 149-31 13-29 30-52-51-113-51-138 0-4 4-7 4-2 35-20t59-34 30-16q8 0 46 46.5t52 67.5q20-2 30-2t30 2q51-71 92-112l6-2q4 0 124 70 4 3 4 7 0 25-51 138 17 23 30 52 149 15 149 31zm0-1024v140q0 16-149 31-12 27-30 52 51 113 51 138 0 4-4 7-122 71-124 71-8 0-46-47t-52-68q-20 2-30 2t-30-2q-14 21-52 68t-46 47q-2 0-124-71-4-3-4-7 0-25 51-138-18-25-30-52-149-15-149-31v-140q0-16 149-31 13-29 30-52-51-113-51-138 0-4 4-7 4-2 35-20t59-34 30-16q8 0 46 46.5t52 67.5q20-2 30-2t30 2q51-71 92-112l6-2q4 0 124 70 4 3 4 7 0 25-51 138 17 23 30 52 149 15 149 31z" fill="#fff"/></svg>|]

-- | https://github.com/Rush/Font-Awesome-SVG-PNG/blob/master/black/svg/question-circle.svg
helpIcon  = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="#FFF"><path d="M0 0h24v24H0V0z" fill="none"/><path d="M11 18h2v-2h-2v2zm1-16C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8zm0-14c-2.21 0-4 1.79-4 4h2c0-1.1.9-2 2-2s2 .9 2 2c0 2-3 1.75-3 5h2c0-2.25 3-2.5 3-5 0-2.21-1.79-4-4-4z"/></svg>|]

isBasicEdition = Version.ihpEdition == Version.Basic