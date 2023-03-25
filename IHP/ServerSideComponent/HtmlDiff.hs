{-|
Module: IHP.ServerSideComponent.HtmlDiff
Copyright: (c) digitally induced GmbH, 2021
Description: Provides differences and patchsets between two HTML fragments
-}
module IHP.ServerSideComponent.HtmlDiff where

import IHP.Prelude
import IHP.ServerSideComponent.HtmlParser
import qualified Data.Text as Text

data NodeOperation
    = UpdateTextContent { textContent :: !Text, path :: ![Int] }
    | ReplaceNode { oldNode :: !Node, newNode :: !Node, newNodeHtml :: !Text, path :: ![Int] }
    | UpdateNode { attributeOperations :: ![AttributeOperation], path :: ![Int] }
    | UpdateComment { comment :: !Text, path :: ![Int] }
    | UpdatePreEscapedTextNode { textContent :: !Text, path :: ![Int] }
    | DeleteNode { node :: !Node, path :: ![Int] }
    | CreateNode { html :: !Text, path :: ![Int] }
    deriving (Eq, Show)

data AttributeOperation
    = UpdateAttribute { attributeName :: !Text, attributeValue :: !Text }
    | AddAttribute { attributeName :: !Text, attributeValue :: !Text }
    | DeleteAttribute { attributeName :: !Text }
    deriving (Eq, Show)

diffHtml :: Text -> Text -> Either _ [NodeOperation]
diffHtml a b = do
    nodeA <- parseHtml a
    nodeB <- parseHtml b

    let ?oldHtml = a
    let ?newHtml = b

    pure (diffNodes nodeA nodeB)

type Path = [Int]

diffNodes :: (?oldHtml :: text, ?newHtml :: Text) => Node -> Node -> [NodeOperation]
diffNodes = diffNodes' []

diffNodes' :: (?oldHtml :: text, ?newHtml :: Text) => Path -> Node -> Node -> [NodeOperation]
diffNodes' path TextNode { textContent = oldTextContent } TextNode { textContent = newTextContent } =
        if oldTextContent == newTextContent
            then []
            else [UpdateTextContent { textContent = newTextContent, path }]
diffNodes' path CommentNode { comment = oldComment } CommentNode { comment = newComment } =
        if oldComment == newComment
            then []
            else [UpdateComment { comment = newComment, path }]
diffNodes' path PreEscapedTextNode { textContent = oldTextContent } PreEscapedTextNode { textContent = newTextContent } =
        if oldTextContent == newTextContent
            then []
            else [UpdatePreEscapedTextNode { textContent = newTextContent, path }]
diffNodes' path oldNode@(Node { tagName = oldTagName, attributes = oldAttributes, children = oldChildren }) newNode@(Node { tagName = newTagName, attributes = newAttributes, children = newChildren }) =
    if oldTagName == newTagName
        then
            let
                attributeOperations = diffAttributes oldAttributes newAttributes
                childrenNodeOperations = diffNodes' path Children { children = oldChildren } Children { children = newChildren }
            in concat [
                    if isEmpty attributeOperations
                        then []
                        else [UpdateNode { attributeOperations, path }]
                    , childrenNodeOperations
                    ]

        else [ReplaceNode { oldNode, newNode, newNodeHtml = nodeOuterHtml newNode ?newHtml, path }]
diffNodes' path Children { children = oldChildren } Children { children = newChildren } =
        let
            patchElements :: [Node] -> [Node] -> Int -> [NodeOperation]
            patchElements (new:nextNewNode:newRest) (old:oldRest) !index | (not $ new `isNodeEqIgnoringPosition` old) && (old `isNodeEqIgnoringPosition` nextNewNode) = [ CreateNode { html = nodeOuterHtml new ?newHtml, path = (index:path) } ] <> (patchElements (newRest) (oldRest) (index + 2)) -- [A, C <old>] -> [A, B <new>, C <nextNewNode>]
            patchElements (new:newRest) (old:nextOld:oldRest) !index | (not $ new `isNodeEqIgnoringPosition` old) && (new `isNodeEqIgnoringPosition` nextOld) = [ DeleteNode { node = old, path = (index:path) } ] <> (patchElements (newRest) (oldRest) (index + 1)) -- [A, B <old>, C <nextOldNode> ] -> [A, C <new>]
            patchElements (new:newRest) (old:oldRest) !index = (diffNodes' (index:path) old new) <> (patchElements newRest oldRest (index + 1))
            patchElements (new:newRest) [] !index = [ CreateNode { html = nodeOuterHtml new ?newHtml, path = (index:path) } ] <> (patchElements newRest [] (index + 1))
            patchElements [] (old:oldRest) !index = [ DeleteNode { node = old, path = (index:path) } ] <> (patchElements [] oldRest (index + 1))
            patchElements [] [] _ = []
        in
            patchElements newChildren oldChildren 0
diffNodes' path oldNode newNode = [ReplaceNode { oldNode, newNode, newNodeHtml = nodeOuterHtml newNode ?newHtml, path }]


diffAttributes :: [Attribute] -> [Attribute] -> [AttributeOperation]
diffAttributes old new = addOrUpdateAttributes <> deleteAttributes
    where
        addOrUpdateAttributes :: [AttributeOperation]
        addOrUpdateAttributes =
                new
                |> map (matchAttribute old)
                |> zip new
                |> mapMaybe diffMatchedAttribute

        deleteAttributes :: [AttributeOperation]
        deleteAttributes =
                old
                |> map (matchAttribute new)
                |> zip old
                |> filter (\(_, newAttribute) -> isNothing newAttribute)
                |> map (\(Attribute { attributeName }, _) -> DeleteAttribute { attributeName })

        diffMatchedAttribute :: (Attribute, Maybe Attribute) -> Maybe AttributeOperation
        diffMatchedAttribute (Attribute { attributeName, attributeValue = newValue }, Just Attribute { attributeValue = oldValue }) | newValue == oldValue = Nothing
        diffMatchedAttribute (Attribute { attributeName, attributeValue = newValue }, Just Attribute { attributeValue = oldValue }) = Just UpdateAttribute { attributeName, attributeValue = newValue }
        diffMatchedAttribute (Attribute { attributeName, attributeValue }, Nothing) = Just AddAttribute { attributeName, attributeValue }

        -- | Finds an attribute in 'old' with the same attribute name
        matchAttribute :: [Attribute] -> Attribute -> Maybe Attribute
        matchAttribute attributes Attribute { attributeName } = find (\Attribute { attributeName = attributeName' } -> attributeName == attributeName' ) attributes

-- | Grabs the entire HTML string corresponding to the node boundaries.
--
-- Node boundaries are only stored for 'Node'. Other nodes ('TextNode', etc) don't store start/end offset, so we render
-- them by ourselves.
nodeOuterHtml :: Node -> Text -> Text
nodeOuterHtml Node { startOffset, endOffset } html = html
        |> Text.drop startOffset
        |> Text.take (endOffset - startOffset)
-- Assuming chars are already escaped, because that's what HSX produces
nodeOuterHtml TextNode { textContent } _ = textContent
nodeOuterHtml PreEscapedTextNode { textContent } _ = textContent
nodeOuterHtml Children { children } html = mconcat $ map (`nodeOuterHtml` html) children
nodeOuterHtml CommentNode { comment } _ = "<!--" <> comment <> "-->"

isNodeEqIgnoringPosition :: Node -> Node -> Bool
isNodeEqIgnoringPosition a@(Node {}) b@(Node {}) = (a { startOffset = 0, endOffset = 0 }) == (b { startOffset = 0, endOffset = 0 })
isNodeEqIgnoringPosition a b = a == b
