module TurboHaskell.ApplicationContext where

import ClassyPrelude
import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy         as Vault
import           TurboHaskell.ModelSupport (ModelContext)

data ApplicationContext = ApplicationContext
    { modelContext :: !ModelContext
    , session :: Vault.Key (Session IO String String)
    }
