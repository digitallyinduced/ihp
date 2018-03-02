module Foundation.ApplicationContext where

import ClassyPrelude (String, IO)
import Network.Wai.Session (Session)
import qualified Data.Vault.Lazy         as Vault
import           Foundation.ModelSupport (ModelContext)

data ApplicationContext = ApplicationContext { modelContext :: ModelContext, session :: Vault.Key (Session IO String String) }

