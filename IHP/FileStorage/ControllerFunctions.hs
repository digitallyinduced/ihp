{-|
Module: IHP.FileStorage.ControllerFunctions
Description: Store uploaded files
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.FileStorage.ControllerFunctions
( storeFile
, storeFileWithOptions
, contentDispositionAttachmentAndFileName
, createTemporaryDownloadUrl
, createTemporaryDownloadUrlFromPath
, uploadToStorageWithOptions
) where

import IHP.Prelude
import IHP.FileStorage.Types
import IHP.Controller.Context
import IHP.Controller.FileUpload
import IHP.FrameworkConfig
import qualified IHP.ModelSupport as ModelSupport
import IHP.ValidationSupport

import Network.Minio
import qualified System.IO.Temp as Temp
import qualified Data.Conduit.Binary as Conduit
import qualified Network.Wai.Parse as Wai

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.TMap as TMap
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Directory
import qualified Control.Exception as Exception
import qualified System.IO.Temp as Temp
import qualified System.Process as Process

-- | Uploads a file to a directory in the storage
--
-- See 'storeFileWithOptions' for more advanced use cases.
--
-- __Example:__ Save a file upload by the user to the storage
--
-- > action UpdateLogoAction = do
-- >     let file = fileOrNothing "file"
-- >             |> fromMaybe (error "No file given")
-- >
-- >     storedFile <- storeFile file "logos"
-- >
-- >     let url = get #url storedFile
-- >
--
storeFile :: (?context :: context, ConfigProvider context) => Wai.FileInfo LByteString -> Text -> IO StoredFile
storeFile fileInfo directory = storeFileWithOptions fileInfo (def { directory })


-- | Like 'storeFile' but with more options.
--
-- See 'storeFileWithOptions' for more advanced use cases.
--
-- __Example:__ Save a file to @my_files@ directory and specify a 'Content-Disposition: attachment; filename="$filename"' header
--
-- > let file = fileOrNothing "file" |> fromMaybe (error "no file given")
-- >
-- > let options :: StoreFileOptions = def
-- >         { directory = "my_files"
-- >         , contentDisposition = contentDispositionAttachmentAndFileName
-- >         }
-- >
-- > storedFile <- storeFileWithOptions file options
-- > let url = get #url storedFile
--
--
-- __Example:__ Transform an uploaded image to a JPEG file, strip meta data and store it inside the @pictures@ directory
--
-- > let file = fileOrNothing "file" |> fromMaybe (error "no file given")
-- >
-- > let options :: StoreFileOptions = def
-- >         { directory = "pictures"
-- >         , preprocess = applyImageMagick "jpg" "-strip"
-- >         }
-- >
-- > storedFile <- storeFileWithOptions file options
-- > let url = get #url storedFile
--
storeFileWithOptions :: (?context :: context, ConfigProvider context) => Wai.FileInfo LByteString -> StoreFileOptions -> IO StoredFile
storeFileWithOptions fileInfo options = do
    objectId <- UUID.nextRandom
    
    let directory = get #directory options
    let objectPath = directory <> "/" <> UUID.toText objectId
    let preprocess = get #preprocess options

    fileInfo <- preprocess fileInfo

    url <- case storage of
        StaticDirStorage -> do
            let destPath :: Text = "static/" <> objectPath
            Directory.createDirectoryIfMissing True (cs $ "static/" <> directory)

            fileInfo
                |> get #fileContent
                |> LBS.writeFile (cs destPath)

            let frameworkConfig = getFrameworkConfig ?context
            pure $ (get #baseUrl frameworkConfig) <> "/" <> objectPath
        S3Storage { connectInfo, bucket, region } -> do
            let payload = fileInfo
                    |> get #fileContent
                    |> Conduit.sourceLbs

            let contentType = cs (Wai.fileContentType fileInfo)
            contentDisposition <- (get #contentDisposition options) fileInfo
            runMinio connectInfo do
                let options :: PutObjectOptions = defaultPutObjectOptions { pooContentType = Just contentType, pooContentDisposition = contentDisposition }
                putObject bucket objectPath payload Nothing options

            pure $ "https://" <> bucket <> ".s3." <> region <> ".amazonaws.com/" <> objectPath
    
    pure StoredFile { path = objectPath, url }

-- | Returns a signed url for a path inside the storage. The url is valid for 7 days.
--
-- If the 'StaticDirStorage' is used, a unsigned normal URL will be returned, as these files are public anyways.
--
-- __Example:__ Get a signed url for a path
--
-- >
-- > signedUrl <- createTemporaryDownloadUrlFromPath "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"
-- >
-- > let url :: Text = get #url signedUrl
-- > let expiredAt :: UTCTime = get #expiredAt signedUrl
--
createTemporaryDownloadUrlFromPath :: (?context :: context, ConfigProvider context) => Text -> IO TemporaryDownloadUrl
createTemporaryDownloadUrlFromPath objectPath = do
    let validInSeconds = 7 * 24 * 3600
    publicUrlExpiredAt <- addUTCTime (fromIntegral validInSeconds) <$> getCurrentTime

    case storage of
        StaticDirStorage -> do
            let frameworkConfig = getFrameworkConfig ?context
            let url = (get #baseUrl frameworkConfig) <> "/" <> objectPath

            pure TemporaryDownloadUrl { url = cs url, expiredAt = publicUrlExpiredAt }
        S3Storage { connectInfo, bucket} -> do
            
            url <- runMinio connectInfo do
                presignedGetObjectUrl bucket objectPath validInSeconds [] []

            case url of
                Left message -> error (tshow message)
                Right url -> pure TemporaryDownloadUrl { url = cs url, expiredAt = publicUrlExpiredAt }

-- | Returns a signed url for a 'StoredFile'. The url is valid for 7 days.
--
-- If the 'StaticDirStorage' is used, a unsigned normal URL will be returned, as these files are public anyways.
--
-- __Example:__ Get a signed url for a stored file using 'createTemporaryDownloadUrl'
--
-- > let file = fileOrNothing "file"
-- >         |> fromMaybe (error "No file given")
-- >
-- > storedFile <- storeFile file "logos"
-- >
-- > signedUrl <- createTemporaryDownloadUrl storedFile
-- >
-- > let url :: Text = get #url signedUrl
-- > let expiredAt :: UTCTime = get #expiredAt signedUrl
--
createTemporaryDownloadUrl :: (?context :: context, ConfigProvider context) => StoredFile -> IO TemporaryDownloadUrl
createTemporaryDownloadUrl storedFile = createTemporaryDownloadUrlFromPath (get #path storedFile)

contentDispositionAttachmentAndFileName :: Wai.FileInfo LByteString -> IO (Maybe Text)
contentDispositionAttachmentAndFileName fileInfo = pure (Just ("attachment; filename=\"" <> cs (get #fileName fileInfo) <> "\""))

-- | Saves an upload to the storage and sets the record attribute to the url.
--
-- __Example:__ Upload a logo for a Company and convert it to a 512x512 PNG
-- 
-- > action UpdateCompanyAction { companyId } = do
-- >     let uploadLogo = uploadToStorageWithOptions $ def
-- >             { preprocess = applyImageMagick "png" "-resize '512x512^' -gravity north -extent 512x512 -quality 100% -strip"  }
-- > 
-- >     company <- fetch companyId
-- >     company
-- >         |> fill @'["name"]
-- >         |> uploadLogo #logoUrl
-- >         >>= ifValid \case
-- >             Left company -> render EditView { .. }
-- >             Right company -> do
-- >                 company <- company |> updateRecord
-- >                 redirectTo EditCompanyAction { .. }
--
uploadToStorageWithOptions :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?context :: ControllerContext
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , Show (ModelSupport.PrimaryKey (ModelSupport.GetTableName (ModelSupport.NormalizeModel record)))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
        , HasField "meta" record MetaBag
        , SetField "meta" record MetaBag
    ) => StoreFileOptions -> Proxy fieldName -> record -> IO record
uploadToStorageWithOptions options field record = do
    let fieldName :: ByteString = cs (symbolVal (Proxy @fieldName))
    let tableName :: Text = cs (symbolVal (Proxy @tableName))
    let directory = tableName <> "/" <> cs fieldName

    case fileOrNothing fieldName of
        Just fileInfo | not (LBS.null (get #fileContent fileInfo)) -> do
            storeFileWithOptions fileInfo options { directory }
            |> Exception.try
            >>= \case
                Left (exception :: SomeException) -> record
                            |> attachFailure field ("Failed uploading to storage: " <> show exception)
                            |> pure
                Right storedFile -> record
                            |> setField @fieldName (Just (get #url storedFile))
                            |> pure

        Nothing -> pure record

-- | Saves an upload to the storage and sets the record attribute to the url.
--
-- Uses the table name of the record as the upload directory (e.g. @companies@ when saving an attachment for a @Company@ record).
--
-- See 'uploadToStorageWithOptions' if you want to provide custom options.
--
-- __Example:__ Upload a logo for a Company
-- 
-- > action UpdateCompanyAction { companyId } = do
-- >     company <- fetch companyId
-- >     company
-- >         |> fill @'["name"]
-- >         |> uploadToStorage #logoUrl
-- >         >>= ifValid \case
-- >             Left company -> render EditView { .. }
-- >             Right company -> do
-- >                 company <- company |> updateRecord
-- >                 redirectTo EditCompanyAction { .. }
--
uploadToStorage :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?context :: ControllerContext
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , Show (ModelSupport.PrimaryKey (ModelSupport.GetTableName (ModelSupport.NormalizeModel record)))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
        , HasField "meta" record MetaBag
        , SetField "meta" record MetaBag
    ) => Proxy fieldName -> record -> IO record
uploadToStorage field record = uploadToStorageWithOptions def field record

-- | Returns the current storage configured in Config.hs
storage :: (?context :: context, ConfigProvider context) => FileStorage
storage = getFrameworkConfig ?context
        |> get #appConfig
        |> TMap.lookup @FileStorage
        |> fromMaybe (error "Could not find FileStorage in config. Did you call initS3Storage from your Config.hs?")