module IHP.Controller.AccessDenied
( accessDeniedWhen
, accessDeniedUnless
, handleAccessDeniedFound
, buildAccessDeniedResponse
, renderAccessDenied
)
where

import IHP.Prelude hiding (displayException)
import IHP.Controller.RequestContext
import Network.HTTP.Types (status403)
import Network.Wai
import Network.HTTP.Types.Header
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze
import qualified Data.ByteString.Lazy as LBS
import IHP.HSX.QQ (hsx)
import qualified System.Directory as Directory
import IHP.Controller.Context
import IHP.Controller.Response (respondAndExit)


-- | Stops the action execution with an access denied message (403) when the access condition is True.
--
-- __Example:__ Checking a user is the author of a blog post.
--
-- > action EditPostAction { postId } = do
-- >     post <- fetch postId
-- >     accessDeniedWhen (post.authorId /= currentUserId)
-- >
-- >     renderHtml EditView { .. }
--
-- This will throw an error and prevent the view from being rendered when the current user is not the author of the post.
accessDeniedWhen :: (?context :: ControllerContext) => Bool -> IO ()
accessDeniedWhen condition = when condition renderAccessDenied

-- | Stops the action execution with an access denied message (403) when the access condition is False.
--
-- __Example:__ Checking a user is the author of a blog post.
--
-- > action EditPostAction { postId } = do
-- >     post <- fetch postId
-- >     accessDeniedUnless (post.authorId == currentUserId)
-- >
-- >     renderHtml EditView { .. }
--
-- This will throw an error and prevent the view from being rendered when the current user is not the author of the post.
accessDeniedUnless :: (?context :: ControllerContext) => Bool -> IO ()
accessDeniedUnless condition = unless condition renderAccessDenied

-- | Renders a 403 access denied response. If a static/403.html exists, that is rendered instead of the IHP access denied page.
handleAccessDeniedFound :: Request -> Respond -> IO ResponseReceived
handleAccessDeniedFound request respond = do
    response <- buildAccessDeniedResponse
    respond response

buildAccessDeniedResponse :: IO Response
buildAccessDeniedResponse = do
    hasCustomAccessDenied <- Directory.doesFileExist "static/403.html"
    if hasCustomAccessDenied
        then customAccessDeniedResponse
        else pure defaultAccessDeniedResponse

-- | The default IHP 403 not found page
defaultAccessDeniedResponse :: Response
defaultAccessDeniedResponse = responseBuilder status403 [(hContentType, "text/html")] $ Blaze.renderHtmlBuilder [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>

        <title>Access denied</title>
        <style>
        @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@500;900&display=swap');
        html, body {
            height: 100%;
            font-family: 'Poppins', sans-serif;
        }
        body {
            color: white;
            letter-spacing: 1.01px;
            text-align: center;
            background-image: linear-gradient(59deg, #044d60 3%, #0082a3 98%);
            display: grid;
            place-items: center;
            margin: 0;
        }
        div {
            width: 796px;
            max-width: 90vw;
        }
        hr {
            border: none;
            border-top: 1px solid white;
        }
        h1 {
            font-weight: 900;
            margin: 24px 0 12px 0;
            font-size: 31px;
        }
        p {
            font-weight: 500;
            margin: 0;
            font-size: 31px;
        }
        </style>
    </head>
    <body>
        <div>
            <svg width="229px" height="124px" viewBox="0 0 458 248">
                <defs>
                    <filter color-interpolation-filters="auto" id="filter-1">
                        <feColorMatrix in="SourceGraphic" type="matrix" values="0 0 0 0 1.000000 0 0 0 0 1.000000 0 0 0 0 1.000000 0 0 0 1.000000 0"></feColorMatrix>
                    </filter>
                </defs>
                <g id="Logo-Showcase" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd">
                    <g filter="url(#filter-1)" id="Group-2">
                        <g>
                            <g id="Group">
                                <path d="M66.05902,0 L104.505158,0 C105.28742,2.63545372e-15 105.997877,0.456069021 106.323569,1.1673062 L194.702842,194.167306 C195.162726,195.171587 194.721406,196.358528 193.717125,196.818412 C193.45583,196.938065 193.171818,197 192.884431,197 L150.564632,197 C149.770251,197 149.051173,196.529864 148.732554,195.80218 L64.2269428,2.80218032 C63.7839108,1.79035204 64.2450114,0.610954791 65.2568397,0.167922835 C65.5097746,0.057174527 65.7829017,-3.93367112e-16 66.05902,0 Z" id="Path-4" fill="#026B86"></path>
                                <path d="M65.8632635,98 L103.588393,98 C105.245247,98 106.588393,99.3431458 106.588393,101 C106.588393,101.562078 106.430486,102.11285 106.13267,102.589544 L48.0306793,195.589544 C47.4825198,196.466947 46.5209615,197 45.4864016,197 L5.52260972,197 C3.86575547,197 2.52260972,195.656854 2.52260972,194 C2.52260972,193.420447 2.69047977,192.8533 3.00592763,192.367116 L63.3465814,99.3671157 C63.8997523,98.5145414 64.846956,98 65.8632635,98 Z" id="Path-2" fill="#063642"></path>
                            </g>
                            <path d="M239.055,197 L239.055,98.588 L215.147,98.588 L215.147,197 L239.055,197 Z M287.6355,197 L287.6355,155.856 L325.9995,155.856 L325.9995,197 L349.7685,197 L349.7685,98.588 L325.9995,98.588 L325.9995,135.84 L287.6355,135.84 L287.6355,98.588 L263.8665,98.588 L263.8665,197 L287.6355,197 Z M398.349,197 L398.349,159.887 L410.164,159.887 C415.260667,159.887 420.172,159.331 424.898,158.219 C429.624,157.107 433.794,155.346333 437.408,152.937 C441.022,150.527667 443.894667,147.353833 446.026,143.4155 C448.157333,139.477167 449.223,134.635333 449.223,128.89 C449.223,123.237333 448.226833,118.488167 446.2345,114.6425 C444.242167,110.796833 441.5085,107.6925 438.0335,105.3295 C434.5585,102.9665 430.481167,101.252167 425.8015,100.1865 C421.121833,99.1208333 416.094667,98.588 410.72,98.588 L410.72,98.588 L374.58,98.588 L374.58,197 L398.349,197 Z M408.357,140.983 L398.349,140.983 L398.349,117.77 L408.774,117.77 C410.905333,117.77 412.967167,117.909 414.9595,118.187 C416.951833,118.465 418.735667,119.021 420.311,119.855 C421.886333,120.689 423.137333,121.847333 424.064,123.33 C424.990667,124.812667 425.454,126.712333 425.454,129.029 C425.454,131.345667 424.990667,133.2685 424.064,134.7975 C423.137333,136.3265 421.863167,137.554333 420.2415,138.481 C418.619833,139.407667 416.789667,140.056333 414.751,140.427 C412.712333,140.797667 410.581,140.983 408.357,140.983 L408.357,140.983 Z" id="IHP" fill="#000000" fill-rule="nonzero"></path>
                        </g>
                    </g>
                </g>
            </svg>
            <hr />
            <h1>Error 403</h1>
            <p>Access denied</p>
        </div>
    </body>
</html>
    |]

-- | Renders the static/403.html file
customAccessDeniedResponse :: IO Response
customAccessDeniedResponse = do
    -- We cannot use responseFile here as responseFile ignore the status code by default
    --
    -- See https://github.com/yesodweb/wai/issues/644
    page <- LBS.readFile "static/403.html"
    pure $ responseLBS status403 [(hContentType, "text/html")] page


-- | Renders an "Access denied" page.
--
-- This can be useful e.g. when an entity cannot be accessed:
--
-- > action ExampleAction = do
-- >     renderAccessDenied
--
-- You can override the default access denied page by creating a new file at @static/403.html@. Then IHP will render that HTML file instead of displaying the default IHP access denied page.
--
renderAccessDenied :: (?context :: ControllerContext) => IO ()
renderAccessDenied = do
    response <- buildAccessDeniedResponse
    respondAndExit response