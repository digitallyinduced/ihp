{-|
Module: IHP.FileStorage.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.FileStorage.Types
( FileStorage (..)
, StoredFile (..)
, StoreFileOptions (..)
, TemporaryDownloadUrl (..)
) where

import IHP.Prelude
import qualified Network.Minio as Minio
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai.Parse as Wai

data FileStorage
    = StaticDirStorage -- ^ Stores files publicly visible inside the project's @static@ directory
    | S3Storage { connectInfo :: Minio.ConnectInfo, bucket :: Text, region :: Text } -- ^ Stores files inside a S3 compatible cloud storage

-- | Result of a 'storeFile' operation
data StoredFile = StoredFile { path :: Text, url :: Text }

-- | Options that can be passed to 'storeFileWithOptions'
data StoreFileOptions = StoreFileOptions
    { directory :: Text -- ^ Directory on S3 or inside the @static@ where all files are placed
    , contentDisposition :: Wai.FileInfo LByteString -> IO (Maybe Text) -- ^ The browser uses the content disposition header to detect if the file should be shown inside the browser or should be downloaded as a file attachment. You can provide a function here that returns a custom content-disposition header based on the uploaded file. This currently only works with the S3 storage. See 'contentDispositionAttachmentAndFileName' for standard configuration.
    , preprocess :: Wai.FileInfo LByteString -> IO (Wai.FileInfo LByteString) -- ^ Can be used to preprocess the file before storing it inside the storage. See 'applyImageMagick' for preprocessing images.
    }

instance Default StoreFileOptions where
    def = StoreFileOptions
        { directory = ""
        , contentDisposition = const (pure Nothing)
        , preprocess = pure
        }

-- | A signed url to a file. See 'createTemporaryDownloadUrl'
data TemporaryDownloadUrl = TemporaryDownloadUrl { url :: Text, expiredAt :: UTCTime }