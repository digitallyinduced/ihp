{-|
Module: IHP.FileStorage.Config
Description: Helpers for Config.hs to set up the File Storage
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.FileStorage.Config
( initS3Storage
, initStaticDirStorage
, initMinioStorage
, initFilebaseStorage
) where

import IHP.Prelude
import IHP.FileStorage.Types
import IHP.FrameworkConfig

import Network.Minio

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.TMap as TMap
import qualified System.Environment as Env
import qualified Data.Text as Text
import Control.Monad.Trans.Maybe

-- | The AWS access key and secret key have to be provided using the @AWS_ACCESS_KEY_ID@ and @AWS_SECRET_ACCESS_KEY@ env vars.
--
-- __Example:__ Set up a s3 storage in @Config.hs@
--
-- > module Config where
-- >
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.FileStorage.Config
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initS3Storage "eu-central-1" "my-bucket-name"
--
initS3Storage :: Text -> Text -> State.StateT TMap.TMap IO ()
initS3Storage region bucket = do
    connectInfo <- awsCI
        |> setRegion region
        |> setCredsFrom [fromAWSEnv]
        |> liftIO

    let baseUrl = "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com/"
    option S3Storage { connectInfo, bucket, baseUrl }

-- | The Minio access key and secret key have to be provided using the @MINIO_ACCESS_KEY@ and @MINIO_SECRET_KEY@ env vars.
--
-- __Example:__ Set up a minio storage in @Config.hs@
--
-- > module Config where
-- >
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.FileStorage.Config
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initMinioStorage "https://minio.example.com" "my-bucket-name"
--
initMinioStorage :: Text -> Text -> State.StateT TMap.TMap IO ()
initMinioStorage server bucket = do
    connectInfo <- server
        |> cs
        |> fromString
        |> setCredsFrom [fromMinioEnv]
        |> liftIO

    let baseUrl = server <> "/" <> bucket <> "/"
    option S3Storage { connectInfo, bucket, baseUrl }

-- | Stores files publicly visible inside the @static@ directory
--
-- __Example:__ Store uploaded files in the @static/@ directory
--
-- > module Config where
-- >
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.FileStorage.Config
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initStaticDirStorage
--
initStaticDirStorage :: State.StateT TMap.TMap IO ()
initStaticDirStorage = option StaticDirStorage

-- | The Filebase access key and secret key have to be provided using the @FILEBASE_KEY@ and @FILEBASE_SECRET@ env vars.
--
-- __Example:__ Set up a Filebase storage in @Config.hs@
--
-- > module Config where
-- >
-- > import IHP.Prelude
-- > import IHP.Environment
-- > import IHP.FrameworkConfig
-- > import IHP.FileStorage.Config
-- >
-- > config :: ConfigBuilder
-- > config = do
-- >     option Development
-- >     option (AppHostname "localhost")
-- >     initFilebaseStorage "my-bucket-name"
--
initFilebaseStorage :: Text -> State.StateT TMap.TMap IO ()
initFilebaseStorage bucket = do
    connectInfo <- filebaseCI
        |> setCredsFrom [fromFilebaseEnv]
        |> liftIO

    let baseUrl = "https://" <> bucket <> ".s3.filebase.com/"
    option S3Storage { connectInfo, bucket, baseUrl }

filebaseCI :: ConnectInfo
filebaseCI = "https://s3.filebase.com" |> setRegion "us-east-1"

fromFilebaseEnv :: Provider
fromFilebaseEnv = runMaybeT $ do
    filebaseKey <- MaybeT $ Env.lookupEnv "FILEBASE_KEY"
    filebaseSecret <- MaybeT $ Env.lookupEnv "FILEBASE_SECRET"
    pure $ Credentials (Text.pack filebaseKey) (Text.pack filebaseSecret)
