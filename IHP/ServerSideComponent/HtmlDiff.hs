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
    | ReplaceNode { oldNode :: !Node, newNode :: !Node, path :: ![Int] }
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

        else [ReplaceNode { oldNode, newNode, path }]
diffNodes' path Children { children = oldChildren } Children { children = newChildren } =
        let
            infiniteNothings :: [Maybe Node]
            infiniteNothings = (map (const Nothing) ([0..] :: [Int]))

            maybeOldPadded = (map Just oldChildren) <> infiniteNothings
            updateOrCreateNode (new, Just old, index) = diffNodes' (index:path) old new
            updateOrCreateNode (new, Nothing, index) = [ CreateNode { html = nodeOuterHtml new ?newHtml, path = (index:path) } ]

            updatedOrNewChildren =
                    zip3 newChildren maybeOldPadded [0..]
                    |> map updateOrCreateNode
                    |> concat


            deletedChildren =
                if length oldChildren > length newChildren
                    then oldChildren
                            |> zip [0..]
                            |> drop (length newChildren)
                            |> map (\(index, node) -> DeleteNode { node = node, path = (index:path) })
                    else []
        in
            updatedOrNewChildren <> deletedChildren
diffNodes' path oldNode newNode = [ReplaceNode { oldNode, newNode, path }]


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

nodeOuterHtml :: Node -> Text -> Text
nodeOuterHtml Node { startOffset, endOffset } html = html
        |> Text.drop startOffset
        |> Text.take (endOffset - startOffset)