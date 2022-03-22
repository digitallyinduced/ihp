module IHP.IDE.ToolServer.Layout where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes ()
import qualified IHP.Version as Version
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import IHP.IDE.ToolServer.Helper.View

toolServerLayout :: Html -> Html
toolServerLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"/>

    <link rel="shortcut icon" type="image/x-icon" href="/ihp-icon-white-bg.svg"/>
    <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
    <link rel="stylesheet" href={assetPath "/IDE/schema-designer.css"}/>
    <link rel="stylesheet" href={assetPath "/vendor/select2.min.css"}/>

    <link rel="stylesheet" href={assetPath "/IDE/Graph/public/app.css"}/>

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

    <script src={assetPath "/vendor/clipboard.min.js"}></script>
    <script src={assetPath "/IDE/clipboard.js"}></script>

    <script src={assetPath "/IDE/Graph/public/app.js"}></script>


    <title>IHP IDE</title>
</head>
<body class="d-flex h-100 flex-row">
    <div id="nav">
        <img id="nav-logo" src="/ihp-icon.svg" alt="IHP: Integrated Haskell Platform">
        <div id="ihp-plan">{ihpEditionTitle}</div>
        {schema}
        {data_}
        {graph}
        {logs}

        {help}
        <a href="https://www.digitallyinduced.com/" id="nav-copyright" target="_blank">©<br />digitally induced GmbH</a>
    </div>
    <div id="content">
        {inner}
    </div>
</body>
|]  where
        (AvailableApps appNames) = fromFrozenContext @AvailableApps
        apps = forEach appNames appNavItem
        schema = navItem "SCHEMA" schemaIcon (pathTo TablesAction) (isSchemaEditorController)
        data_ = navItem "DATA" dataIcon (pathTo ShowDatabaseAction) (isActiveController @DataController)
        graph = navItem "GRAPH" graphIcon (pathTo ExploreAction) (isActiveController @GraphController)
        repl = navItem "REPL" terminalIcon "#" False
        codegen = navItem "CODEGEN" copyIcon (pathTo GeneratorsAction) (isActiveController @CodeGenController)
        logs = navItem "LOGS" serverIcon (pathTo AppLogsAction) (isActiveController @LogsController)
        lint = navItem "LINT" flagIcon "#" False
        deploy = navItem "DEPLOY" globeIcon "https://ihpcloud.com/" False
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
                data-title="Questions, or need help with GraphQL errors?"
                data-trigger="focus"
                id="nav-help"
            >
                {helpIcon}
            </a>
            <div id="help-content" style="display: none">
                <a class="btn btn-dark btn-block mb-1" href="https://github.com/digitallyinduced/thin-backend/discussions" target="_blank">
                    → GitHub Discussions
                </a>

                <p class="text-muted text-center">
                    <small>
                        Thin Version: {Version.ihpVersion}
                    </small>
                </p>

                <p class="text-muted email-support">
                    If you have a Support Subscription, you can also <a href="mailto:support@digitallyinduced.com">reach out to the digitally induced email support</a>.
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
            Version.Basic -> [hsx|IHP <br />GraphQL|]
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

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/globe.svg
globeIcon = preEscapedToHtml [plain|<svg viewBox="0 0 1792 1792" xmlns="http://www.w3.org/2000/svg"><path d="M896 128q209 0 385.5 103t279.5 279.5 103 385.5-103 385.5-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103zm274 521q-2 1-9.5 9.5t-13.5 9.5q2 0 4.5-5t5-11 3.5-7q6-7 22-15 14-6 52-12 34-8 51 11-2-2 9.5-13t14.5-12q3-2 15-4.5t15-7.5l2-22q-12 1-17.5-7t-6.5-21q0 2-6 8 0-7-4.5-8t-11.5 1-9 1q-10-3-15-7.5t-8-16.5-4-15q-2-5-9.5-11t-9.5-10q-1-2-2.5-5.5t-3-6.5-4-5.5-5.5-2.5-7 5-7.5 10-4.5 5q-3-2-6-1.5t-4.5 1-4.5 3-5 3.5q-3 2-8.5 3t-8.5 2q15-5-1-11-10-4-16-3 9-4 7.5-12t-8.5-14h5q-1-4-8.5-8.5t-17.5-8.5-13-6q-8-5-34-9.5t-33-.5q-5 6-4.5 10.5t4 14 3.5 12.5q1 6-5.5 13t-6.5 12q0 7 14 15.5t10 21.5q-3 8-16 16t-16 12q-5 8-1.5 18.5t10.5 16.5q2 2 1.5 4t-3.5 4.5-5.5 4-6.5 3.5l-3 2q-11 5-20.5-6t-13.5-26q-7-25-16-30-23-8-29 1-5-13-41-26-25-9-58-4 6-1 0-15-7-15-19-12 3-6 4-17.5t1-13.5q3-13 12-23 1-1 7-8.5t9.5-13.5.5-6q35 4 50-11 5-5 11.5-17t10.5-17q9-6 14-5.5t14.5 5.5 14.5 5q14 1 15.5-11t-7.5-20q12 1 3-17-4-7-8-9-12-4-27 5-8 4 2 8-1-1-9.5 10.5t-16.5 17.5-16-5q-1-1-5.5-13.5t-9.5-13.5q-8 0-16 15 3-8-11-15t-24-8q19-12-8-27-7-4-20.5-5t-19.5 4q-5 7-5.5 11.5t5 8 10.5 5.5 11.5 4 8.5 3q14 10 8 14-2 1-8.5 3.5t-11.5 4.5-6 4q-3 4 0 14t-2 14q-5-5-9-17.5t-7-16.5q7 9-25 6l-10-1q-4 0-16 2t-20.5 1-13.5-8q-4-8 0-20 1-4 4-2-4-3-11-9.5t-10-8.5q-46 15-94 41 6 1 12-1 5-2 13-6.5t10-5.5q34-14 42-7l5-5q14 16 20 25-7-4-30-1-20 6-22 12 7 12 5 18-4-3-11.5-10t-14.5-11-15-5q-16 0-22 1-146 80-235 222 7 7 12 8 4 1 5 9t2.5 11 11.5-3q9 8 3 19 1-1 44 27 19 17 21 21 3 11-10 18-1-2-9-9t-9-4q-3 5 .5 18.5t10.5 12.5q-7 0-9.5 16t-2.5 35.5-1 23.5l2 1q-3 12 5.5 34.5t21.5 19.5q-13 3 20 43 6 8 8 9 3 2 12 7.5t15 10 10 10.5q4 5 10 22.5t14 23.5q-2 6 9.5 20t10.5 23q-1 0-2.5 1t-2.5 1q3 7 15.5 14t15.5 13q1 3 2 10t3 11 8 2q2-20-24-62-15-25-17-29-3-5-5.5-15.5t-4.5-14.5q2 0 6 1.5t8.5 3.5 7.5 4 2 3q-3 7 2 17.5t12 18.5 17 19 12 13q6 6 14 19.5t0 13.5q9 0 20 10.5t17 19.5q5 8 8 26t5 24q2 7 8.5 13.5t12.5 9.5l16 8 13 7q5 2 18.5 10.5t21.5 11.5q10 4 16 4t14.5-2.5 13.5-3.5q15-2 29 15t21 21q36 19 55 11-2 1 .5 7.5t8 15.5 9 14.5 5.5 8.5q5 6 18 15t18 15q6-4 7-9-3 8 7 20t18 10q14-3 14-32-31 15-49-18 0-1-2.5-5.5t-4-8.5-2.5-8.5 0-7.5 5-3q9 0 10-3.5t-2-12.5-4-13q-1-8-11-20t-12-15q-5 9-16 8t-16-9q0 1-1.5 5.5t-1.5 6.5q-13 0-15-1 1-3 2.5-17.5t3.5-22.5q1-4 5.5-12t7.5-14.5 4-12.5-4.5-9.5-17.5-2.5q-19 1-26 20-1 3-3 10.5t-5 11.5-9 7q-7 3-24 2t-24-5q-13-8-22.5-29t-9.5-37q0-10 2.5-26.5t3-25-5.5-24.5q3-2 9-9.5t10-10.5q2-1 4.5-1.5t4.5 0 4-1.5 3-6q-1-1-4-3-3-3-4-3 7 3 28.5-1.5t27.5 1.5q15 11 22-2 0-1-2.5-9.5t-.5-13.5q5 27 29 9 3 3 15.5 5t17.5 5q3 2 7 5.5t5.5 4.5 5-.5 8.5-6.5q10 14 12 24 11 40 19 44 7 3 11 2t4.5-9.5 0-14-1.5-12.5l-1-8v-18l-1-8q-15-3-18.5-12t1.5-18.5 15-18.5q1-1 8-3.5t15.5-6.5 12.5-8q21-19 15-35 7 0 11-9-1 0-5-3t-7.5-5-4.5-2q9-5 2-16 5-3 7.5-11t7.5-10q9 12 21 2 8-8 1-16 5-7 20.5-10.5t18.5-9.5q7 2 8-2t1-12 3-12q4-5 15-9t13-5l17-11q3-4 0-4 18 2 31-11 10-11-6-20 3-6-3-9.5t-15-5.5q3-1 11.5-.5t10.5-1.5q15-10-7-16-17-5-43 12zm-163 877q206-36 351-189-3-3-12.5-4.5t-12.5-3.5q-18-7-24-8 1-7-2.5-13t-8-9-12.5-8-11-7q-2-2-7-6t-7-5.5-7.5-4.5-8.5-2-10 1l-3 1q-3 1-5.5 2.5t-5.5 3-4 3 0 2.5q-21-17-36-22-5-1-11-5.5t-10.5-7-10-1.5-11.5 7q-5 5-6 15t-2 13q-7-5 0-17.5t2-18.5q-3-6-10.5-4.5t-12 4.5-11.5 8.5-9 6.5-8.5 5.5-8.5 7.5q-3 4-6 12t-5 11q-2-4-11.5-6.5t-9.5-5.5q2 10 4 35t5 38q7 31-12 48-27 25-29 40-4 22 12 26 0 7-8 20.5t-7 21.5q0 6 2 16z" fill="#fff"/></svg>|]

-- | https://github.com/encharm/Font-Awesome-SVG-PNG/blob/master/white/svg/cogs.svg
cogsIcon = preEscapedToHtml [plain|<svg viewBox="0 0 2048 1792" xmlns="http://www.w3.org/2000/svg"><path d="M960 896q0-106-75-181t-181-75-181 75-75 181 75 181 181 75 181-75 75-181zm768 512q0-52-38-90t-90-38-90 38-38 90q0 53 37.5 90.5t90.5 37.5 90.5-37.5 37.5-90.5zm0-1024q0-52-38-90t-90-38-90 38-38 90q0 53 37.5 90.5t90.5 37.5 90.5-37.5 37.5-90.5zm-384 421v185q0 10-7 19.5t-16 10.5l-155 24q-11 35-32 76 34 48 90 115 7 11 7 20 0 12-7 19-23 30-82.5 89.5t-78.5 59.5q-11 0-21-7l-115-90q-37 19-77 31-11 108-23 155-7 24-30 24h-186q-11 0-20-7.5t-10-17.5l-23-153q-34-10-75-31l-118 89q-7 7-20 7-11 0-21-8-144-133-144-160 0-9 7-19 10-14 41-53t47-61q-23-44-35-82l-152-24q-10-1-17-9.5t-7-19.5v-185q0-10 7-19.5t16-10.5l155-24q11-35 32-76-34-48-90-115-7-11-7-20 0-12 7-20 22-30 82-89t79-59q11 0 21 7l115 90q34-18 77-32 11-108 23-154 7-24 30-24h186q11 0 20 7.5t10 17.5l23 153q34 10 75 31l118-89q8-7 20-7 11 0 21 8 144 133 144 160 0 8-7 19-12 16-42 54t-45 60q23 48 34 82l152 23q10 2 17 10.5t7 19.5zm640 533v140q0 16-149 31-12 27-30 52 51 113 51 138 0 4-4 7-122 71-124 71-8 0-46-47t-52-68q-20 2-30 2t-30-2q-14 21-52 68t-46 47q-2 0-124-71-4-3-4-7 0-25 51-138-18-25-30-52-149-15-149-31v-140q0-16 149-31 13-29 30-52-51-113-51-138 0-4 4-7 4-2 35-20t59-34 30-16q8 0 46 46.5t52 67.5q20-2 30-2t30 2q51-71 92-112l6-2q4 0 124 70 4 3 4 7 0 25-51 138 17 23 30 52 149 15 149 31zm0-1024v140q0 16-149 31-12 27-30 52 51 113 51 138 0 4-4 7-122 71-124 71-8 0-46-47t-52-68q-20 2-30 2t-30-2q-14 21-52 68t-46 47q-2 0-124-71-4-3-4-7 0-25 51-138-18-25-30-52-149-15-149-31v-140q0-16 149-31 13-29 30-52-51-113-51-138 0-4 4-7 4-2 35-20t59-34 30-16q8 0 46 46.5t52 67.5q20-2 30-2t30 2q51-71 92-112l6-2q4 0 124 70 4 3 4 7 0 25-51 138 17 23 30 52 149 15 149 31z" fill="#fff"/></svg>|]

-- | https://github.com/Rush/Font-Awesome-SVG-PNG/blob/master/black/svg/question-circle.svg
helpIcon  = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="#FFF"><path d="M0 0h24v24H0V0z" fill="none"/><path d="M11 18h2v-2h-2v2zm1-16C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.41 0-8-3.59-8-8s3.59-8 8-8 8 3.59 8 8-3.59 8-8 8zm0-14c-2.21 0-4 1.79-4 4h2c0-1.1.9-2 2-2s2 .9 2 2c0 2-3 1.75-3 5h2c0-2.25 3-2.5 3-5 0-2.21-1.79-4-4-4z"/></svg>|]

isBasicEdition = Version.ihpEdition == Version.Basic


graphIcon = preEscapedToHtml [plain|
<?xml version="1.0" encoding="utf-8"?>
<!-- Generator: Adobe Illustrator 18.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0)  -->
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg version="1.1" id="GraphQL_Logo" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px"
     y="0px" viewBox="0 0 400 400" enable-background="new 0 0 400 400" xml:space="preserve">
<g>
    <g>
        <g>
            
                <rect x="122" y="-0.4" transform="matrix(-0.866 -0.5 0.5 -0.866 163.3196 363.3136)" fill="currentColor" width="16.6" height="320.3"/>
        </g>
    </g>
    <g>
        <g>
            <rect x="39.8" y="272.2" fill="currentColor" width="320.3" height="16.6"/>
        </g>
    </g>
    <g>
        <g>
            
                <rect x="37.9" y="312.2" transform="matrix(-0.866 -0.5 0.5 -0.866 83.0693 663.3409)" fill="currentColor" width="185" height="16.6"/>
        </g>
    </g>
    <g>
        <g>
            
                <rect x="177.1" y="71.1" transform="matrix(-0.866 -0.5 0.5 -0.866 463.3409 283.0693)" fill="currentColor" width="185" height="16.6"/>
        </g>
    </g>
    <g>
        <g>
            
                <rect x="122.1" y="-13" transform="matrix(-0.5 -0.866 0.866 -0.5 126.7903 232.1221)" fill="currentColor" width="16.6" height="185"/>
        </g>
    </g>
    <g>
        <g>
            
                <rect x="109.6" y="151.6" transform="matrix(-0.5 -0.866 0.866 -0.5 266.0828 473.3766)" fill="currentColor" width="320.3" height="16.6"/>
        </g>
    </g>
    <g>
        <g>
            <rect x="52.5" y="107.5" fill="currentColor" width="16.6" height="185"/>
        </g>
    </g>
    <g>
        <g>
            <rect x="330.9" y="107.5" fill="currentColor" width="16.6" height="185"/>
        </g>
    </g>
    <g>
        <g>
            
                <rect x="262.4" y="240.1" transform="matrix(-0.5 -0.866 0.866 -0.5 126.7953 714.2875)" fill="currentColor" width="14.5" height="160.9"/>
        </g>
    </g>
    <path fill="currentColor" d="M369.5,297.9c-9.6,16.7-31,22.4-47.7,12.8c-16.7-9.6-22.4-31-12.8-47.7c9.6-16.7,31-22.4,47.7-12.8
        C373.5,259.9,379.2,281.2,369.5,297.9"/>
    <path fill="currentColor" d="M90.9,137c-9.6,16.7-31,22.4-47.7,12.8c-16.7-9.6-22.4-31-12.8-47.7c9.6-16.7,31-22.4,47.7-12.8
        C94.8,99,100.5,120.3,90.9,137"/>
    <path fill="currentColor" d="M30.5,297.9c-9.6-16.7-3.9-38,12.8-47.7c16.7-9.6,38-3.9,47.7,12.8c9.6,16.7,3.9,38-12.8,47.7
        C61.4,320.3,40.1,314.6,30.5,297.9"/>
    <path fill="currentColor" d="M309.1,137c-9.6-16.7-3.9-38,12.8-47.7c16.7-9.6,38-3.9,47.7,12.8c9.6,16.7,3.9,38-12.8,47.7
        C340.1,159.4,318.7,153.7,309.1,137"/>
    <path fill="currentColor" d="M200,395.8c-19.3,0-34.9-15.6-34.9-34.9c0-19.3,15.6-34.9,34.9-34.9c19.3,0,34.9,15.6,34.9,34.9
        C234.9,380.1,219.3,395.8,200,395.8"/>
    <path fill="currentColor" d="M200,74c-19.3,0-34.9-15.6-34.9-34.9c0-19.3,15.6-34.9,34.9-34.9c19.3,0,34.9,15.6,34.9,34.9
        C234.9,58.4,219.3,74,200,74"/>
</g>
</svg>

|]