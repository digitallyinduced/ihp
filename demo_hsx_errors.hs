{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import IHP.HSX.QQ (hsx)

main :: IO ()
main = do
    putStrLn "=== DEMO: HSX Enhanced Error Messages ==="
    putStrLn ""
    putStrLn "Before (Basic Error):"
    putStrLn "  Parse error: unexpected 'spn'"
    putStrLn ""
    putStrLn "After (Enhanced Error):"
    putStrLn "  HSX Parse Error at demo.hs:5:2"
    putStrLn "  Invalid HTML tag: 'spn'"
    putStrLn "  "
    putStrLn "    4 | test = [hsx|<spn>Hello</spn>|]"
    putStrLn "      |             ^"
    putStrLn "  "
    putStrLn "  Suggestions:"
    putStrLn "    • 'spn' is not a valid HTML tag name"
    putStrLn "    • Check if you meant a similar valid tag like: div, span, or check the HTML specification"
    putStrLn "    • For custom web components, use kebab-case with at least one hyphen (e.g., 'my-component')"
    putStrLn "    • Use 'uncheckedHsx' if you need to use non-standard tag names"

-- Example with typo that would trigger our enhanced error
-- test1 = [hsx|<spn>Hello</spn>|]
