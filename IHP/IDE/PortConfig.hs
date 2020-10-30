module IHP.IDE.PortConfig
( PortConfig (..)
, defaultAppPort
, findAvailablePortConfig
)
where

import ClassyPrelude
import qualified Network.Socket as Socket
import qualified UnliftIO.Exception as Exception
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.IO.Exception (IOException(..))
import IHP.FrameworkConfig (defaultPort)

-- | Port configuration used for starting the different app services
data PortConfig = PortConfig
    { appPort :: !Socket.PortNumber
    , toolServerPort :: !Socket.PortNumber
    , liveReloadNotificationPort :: !Socket.PortNumber
    } deriving (Show, Eq)

defaultAppPort :: Socket.PortNumber
defaultAppPort = fromIntegral defaultPort

allPorts :: PortConfig -> [Socket.PortNumber]
allPorts PortConfig { .. } = [appPort, toolServerPort, liveReloadNotificationPort]

instance Enum PortConfig where
    fromEnum PortConfig { .. } = fromIntegral $ toInteger (appPort - defaultAppPort)
    toEnum i = PortConfig { .. }
        where
            port = fromIntegral i
            appPort = port + defaultAppPort
            toolServerPort = port + defaultAppPort + 1
            liveReloadNotificationPort = port + defaultAppPort + 2

-- | Returns True when the given port looks to be free.
-- Used to e.g. detect which port the dev server should use.
isPortAvailable :: Socket.PortNumber -> IO Bool
isPortAvailable port = do
    let address = Socket.SockAddrInet port (Socket.tupleToHostAddress (127, 0, 0, 1))
    Exception.bracket (Socket.socket Socket.AF_INET Socket.Stream 6) Socket.close' $ \socket -> do
        res <- Exception.try (Socket.connect socket address)
        case res of
            Left e -> if (Errno <$> ioe_errno e) == Just eCONNREFUSED
                    then pure True
                    else throwIO e
            Right _ -> pure False

-- | Returns True when all ports in port config are available.
-- 
-- Example:
--
-- >>> let portConfig = PortConfig { appPort = 8000, toolServerPort = 8001, liveReloadNotificationPort = 8002 }
-- >>> isPortConfigAvailable portConfig
-- True
isPortConfigAvailable :: PortConfig -> IO Bool
isPortConfigAvailable portConfig = do
    available <- mapM isPortAvailable (allPorts portConfig)
    pure (and available)

-- | Returns a port config where all ports are available
--
-- When e.g. port 8000, 8001 and 80002 are not used:
--
-- >>> portConfig <- findAvailablePortConfig
-- PortConfig { appPort = 8000, toolServerPort = 8001, liveReloadNotificationPort = 8002 }
findAvailablePortConfig :: IO PortConfig
findAvailablePortConfig = do
        let portConfigs :: [PortConfig] = take 100 (map toEnum [0..])
        go portConfigs
    where
        go (portConfig : rest) = do
            available <- isPortConfigAvailable portConfig
            if available
                then pure portConfig
                else go rest
        go [] = error "findAvailablePortConfig: No port configuration found"