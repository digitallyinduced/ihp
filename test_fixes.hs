-- Test script to verify our bug fixes
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as Text

-- Test the improved attribute extraction logic
extractAttrFromDuplicateMsg :: Text -> Text
extractAttrFromDuplicateMsg msg = 
    -- Look for patterns like "duplicate attribute 'class'" or "attribute: class"
    let words = Text.words msg
        -- Try different patterns for extracting attribute name
        findAttrName [] = "unknown"
        findAttrName (w:ws) = case w of
            "attribute" -> case ws of
                (attr:_) -> Text.strip $ Text.filter (/= '\'') $ Text.filter (/= '"') attr
                [] -> findAttrName ws
            _ -> if "'" `Text.isInfixOf` w || "\"" `Text.isInfixOf` w
                 then Text.strip $ Text.filter (/= '\'') $ Text.filter (/= '"') w
                 else findAttrName ws
    in if Text.null (findAttrName words)
       then "unknown"
       else findAttrName words

-- Test the pointer calculation fix
calculatePointer :: Int -> Int -> Text
calculatePointer maxLineNumLen colNum = 
    Text.replicate (max 0 (maxLineNumLen + 3 + colNum - 1)) " " <> "^"

-- Test cases
main :: IO ()
main = do
    putStrLn "Testing attribute extraction:"
    print $ extractAttrFromDuplicateMsg "duplicate attribute 'class' found"
    print $ extractAttrFromDuplicateMsg "attribute: id already exists"
    print $ extractAttrFromDuplicateMsg "error with \"style\" attribute"
    print $ extractAttrFromDuplicateMsg "some random error message"
    
    putStrLn "\nTesting pointer calculation:"
    print $ calculatePointer 3 5  -- Normal case
    print $ calculatePointer 3 0  -- Edge case that could go negative
    print $ calculatePointer 0 1  -- Another edge case
