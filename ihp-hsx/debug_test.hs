import IHP.HSX.Parser
import Text.Megaparsec
import Data.Text

main :: IO ()
main = do
    case parseHsx defaultSettings (initialPos "") [] "<myinvalidel>" of
        Left e -> do
            let msg = errorBundlePretty e
            putStrLn "ACTUAL ERROR MESSAGE:"
            putStrLn $ show msg
            putStrLn "RAW MESSAGE:"
            putStrLn msg
        Right _ -> putStrLn "success"
