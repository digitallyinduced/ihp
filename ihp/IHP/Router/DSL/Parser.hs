{-|
Module: IHP.Router.DSL.Parser
Description: Hand-rolled parser for the routes DSL

The parser consumes the textual body of a @[routes| ... |]@ block and
produces a 'Routes' AST. Errors carry the source line number so the TH
splice can report them with useful context.

The parser is intentionally hand-rolled — the DSL is small (~7 production
rules) and we want precise control over error messages.
-}
module IHP.Router.DSL.Parser
    ( parseRoutes
    , ParseError (..)
    ) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Char (isUpper, isAlpha, isAlphaNum, isSpace)
import IHP.Router.DSL.AST

-- | Parse error from the DSL parser.
--
-- The 'Int' is the 1-based source line number inside the quasi-quote body
-- (the controller header is line 1).
data ParseError = ParseError
    { errorLine    :: !Int
    , errorMessage :: !Text
    }
    deriving (Eq, Show)

-- | Parse a raw @[routes|...|]@ body into a 'Routes' AST.
--
-- The input is the text between the opening @|@ and the closing @|]@ (the
-- quoter strips those).
--
-- The block may optionally start with a controller-name header — a bare
-- uppercase identifier on its own line before any route lines. If absent,
-- the block is multi-controller and the splice groups routes by reifying
-- each action constructor to find its parent type.
parseRoutes :: Text -> Either ParseError Routes
parseRoutes source = do
    let linesWithNumbers = zip [1 ..] (Text.lines source)
        stripped = filter (not . isBlankOrComment . snd) (map stripComment linesWithNumbers)
    case stripped of
        [] -> Right Routes { controllerName = Nothing, routes = [] }
        allLines@((firstLine, firstText) : restLines)
            | looksLikeHeader firstText -> do
                ctrl <- parseHeader firstLine firstText
                parsedRoutes <- traverse parseRouteLine restLines
                pure Routes { controllerName = Just ctrl, routes = parsedRoutes }
            | otherwise -> do
                parsedRoutes <- traverse parseRouteLine allLines
                pure Routes { controllerName = Nothing, routes = parsedRoutes }

-- | A header line is a single identifier with no whitespace-separated tokens.
-- Route lines have at least three tokens (method, path, action).
looksLikeHeader :: Text -> Bool
looksLikeHeader text =
    let trimmed = Text.strip text
        tokens = Text.words trimmed
     in case tokens of
            [only] -> isValidIdent only && not (isHttpMethodToken only)
            _      -> False
  where
    -- Even a single token could be a malformed route line; reject strings
    -- that look like HTTP methods so e.g. @GET@ alone isn't misread as a
    -- controller named "GET".
    isHttpMethodToken t = t == "ANY" || Text.isInfixOf "|" t
        || case methodFromText t of
            Just _  -> True
            Nothing -> False

-- | Strip an inline @-- ...@ comment and trailing whitespace.
stripComment :: (Int, Text) -> (Int, Text)
stripComment (n, line) =
    let noComment = case Text.breakOn "--" line of
            (before, _)   -> Text.stripEnd before
     in (n, noComment)

isBlankOrComment :: Text -> Bool
isBlankOrComment = Text.null . Text.strip

-- | Parse the controller-name header line.
parseHeader :: Int -> Text -> Either ParseError Text
parseHeader line text =
    let trimmed = Text.strip text
     in if isValidIdent trimmed
            then Right trimmed
            else Left (ParseError line ("routes: invalid controller name: " <> quoted trimmed))

-- | Parse one route line: METHOD(|METHOD)* /path ActionRef
parseRouteLine :: (Int, Text) -> Either ParseError Route
parseRouteLine (line, text) = do
    let trimmed = Text.strip text
    (methodsField, rest1) <- splitFirstToken line trimmed
    methods <- parseMethods line methodsField
    (pathField, rest2) <- splitFirstToken line (Text.strip rest1)
    path <- parsePath line pathField
    action <- parseActionRef line (Text.strip rest2)
    pure Route
        { routeMethods = methods
        , routePath    = path
        , routeAction  = action
        , routeLine    = line
        }

-- | Split a line at the first whitespace, returning (first word, remainder).
splitFirstToken :: Int -> Text -> Either ParseError (Text, Text)
splitFirstToken line text
    | Text.null text = Left (ParseError line "routes: unexpected end of line; expected a token")
    | otherwise = Right (Text.breakOn (Text.singleton ' ') text)

-- | Parse a method field like @GET@, @GET|POST@, or @ANY@.
parseMethods :: Int -> Text -> Either ParseError [Method]
parseMethods line field
    | field == "ANY" = Right expandAnyMethod
    | otherwise = traverse parseOne (Text.splitOn "|" field)
  where
    parseOne raw = case methodFromText raw of
        Just m  -> Right m
        Nothing -> Left (ParseError line ("routes: unknown method: " <> quoted raw))

-- | Parse a path like @/posts/#postId/edit@.
parsePath :: Int -> Text -> Either ParseError [PathSeg]
parsePath line raw
    | Text.null raw = Left (ParseError line "routes: missing path")
    | Text.head raw /= '/' = Left (ParseError line ("routes: path must start with '/': " <> quoted raw))
    | raw == "/" = Right []
    | otherwise =
        let dropped = Text.drop 1 raw
            segments = Text.splitOn "/" dropped
         in traverse (parseSegment line) segments

-- | Parse one path segment: literal, @#name[:Type]@, or @*name[:Type]@.
parseSegment :: Int -> Text -> Either ParseError PathSeg
parseSegment line seg
    | Text.null seg =
        Left (ParseError line "routes: empty path segment (consecutive slashes?)")
    | Text.head seg == '#' = parseNamed Capture (Text.drop 1 seg)
    | Text.head seg == '*' = parseNamed Splat (Text.drop 1 seg)
    | otherwise = Right (Literal seg)
  where
    parseNamed ctor rest = do
        (name, tyRaw) <- case Text.splitOn ":" rest of
            [n]    -> Right (n, Nothing)
            [n, t] -> Right (n, Just t)
            _      -> Left (ParseError line ("routes: too many ':' in segment: " <> quoted seg))
        if isValidIdent name
            then Right (ctor name tyRaw)
            else Left (ParseError line ("routes: invalid capture name: " <> quoted name))

-- | Parse an action reference like @ShowPostAction@ or
-- @ShowMemberAction { organizationId = #org, userId = #user }@.
parseActionRef :: Int -> Text -> Either ParseError ActionRef
parseActionRef line text
    | Text.null text = Left (ParseError line "routes: missing action constructor")
    | otherwise =
        let (nameRaw, after) = Text.break (\c -> isSpace c || c == '{') text
         in if not (isValidIdent nameRaw)
                then Left (ParseError line ("routes: invalid action constructor: " <> quoted nameRaw))
                else case Text.strip after of
                    ""          -> Right (ActionRef nameRaw [])
                    rest
                        | Text.head rest == '{' -> do
                            bindings <- parseBindings line rest
                            pure (ActionRef nameRaw bindings)
                        | otherwise -> Left (ParseError line ("routes: unexpected text after action: " <> quoted rest))

-- | Parse @{ field1 = #cap1, field2 = #cap2 }@.
parseBindings :: Int -> Text -> Either ParseError [(Text, Text)]
parseBindings line raw = do
    body <- case Text.stripPrefix "{" raw of
        Nothing -> Left (ParseError line "routes: expected '{'")
        Just t  -> case Text.stripSuffix "}" (Text.strip t) of
            Nothing -> Left (ParseError line "routes: expected '}' at end of field list")
            Just t' -> Right t'
    traverse parseBinding (Text.splitOn "," body)
  where
    parseBinding b =
        let (fieldRaw, after) = Text.breakOn "=" (Text.strip b)
            field = Text.strip fieldRaw
         in case Text.stripPrefix "=" (Text.strip after) of
                Nothing -> Left (ParseError line ("routes: binding missing '=' in: " <> quoted b))
                Just rhsRaw -> case Text.stripPrefix "#" (Text.strip rhsRaw) of
                    Nothing -> Left (ParseError line ("routes: binding must reference a capture with '#': " <> quoted b))
                    Just captureName
                        | isValidIdent field && isValidIdent captureName ->
                            Right (field, captureName)
                        | otherwise ->
                            Left (ParseError line ("routes: invalid identifier in binding: " <> quoted b))

-- | An identifier: letter-or-underscore followed by alphanumerics/underscores.
isValidIdent :: Text -> Bool
isValidIdent t = case Text.uncons t of
    Nothing      -> False
    Just (h, tl) -> (isAlpha h || h == '_') && Text.all (\c -> isAlphaNum c || c == '_' || c == '\'') tl

quoted :: Text -> Text
quoted t = Text.cons '"' (Text.snoc t '"')

-- | Unused but retained for future validation warnings (e.g. expecting
-- uppercase action names).
_isUpperFirst :: Text -> Bool
_isUpperFirst t = case Text.uncons t of
    Just (h, _) -> isUpper h
    Nothing     -> False
