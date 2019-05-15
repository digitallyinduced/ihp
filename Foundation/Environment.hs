module Foundation.Environment where
import           ClassyPrelude

data Environment = Development | Production deriving (Eq, Show)

isDevelopment :: Environment -> Bool
isDevelopment Development = True
isDevelopment _           = False

isProduction :: Environment -> Bool
isProduction Production = True
isProduction _          = False
