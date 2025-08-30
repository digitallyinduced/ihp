{-|
Module: IHP.FileStorage.ControllerFunctions
Description: Store uploaded files
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.FileStorage.ControllerFunctions
( storeFile
, removeFileFromStorage
, storeFileWithOptions
, storeFileFromUrl
, storeFileFromPath
, contentDispositionAttachmentAndFileName
, createTemporaryDownloadUrl
, createTemporaryDownloadUrlFromPath
, createTemporaryDownloadUrlFromPathWithExpiredAt
, refreshTemporaryDownloadUrlFromFile
, uploadToStorage
, uploadToStorageWithOptions
, storage
, storagePrefix
) where

import IHP.Prelude
import IHP.FileStorage.Types
import IHP.Controller.Context
import IHP.Controller.FileUpload
import IHP.FrameworkConfig
import qualified IHP.ModelSupport as ModelSupport
import IHP.ValidationSupport

import Network.Minio
import qualified Data.Conduit.Binary as Conduit
import qualified Network.Wai.Parse as Wai

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.TMap as TMap
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified System.Directory as Directory
import qualified Control.Exception.Safe as Exception
import qualified Network.Wreq as Wreq
import Control.Lens hiding ((|>), set)
import qualified Network.Mime as Mime

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
-- >     let url = storedFile.url
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
-- > let url = storedFile.url
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
-- > let url = storedFile.url
--
storeFileWithOptions :: (?context :: context, ConfigProvider context) => Wai.FileInfo LByteString -> StoreFileOptions -> IO StoredFile
storeFileWithOptions fileInfo options = do
    objectId <- UUID.nextRandom

    let fileName = options.fileName |> fromMaybe objectId

    let objectPath = options.directory <> "/" <> UUID.toText fileName
    let preprocess = options.preprocess

    fileInfo <- preprocess fileInfo

    url <- case storage of
        StaticDirStorage { directory } -> do
            let destPath :: Text = directory <> objectPath
            Directory.createDirectoryIfMissing True (cs $ directory <> options.directory)

            fileInfo
                |> (.fileContent)
                |> LBS.writeFile (cs destPath)

            let frameworkConfig = ?context.frameworkConfig
            -- Prefix with a slash so it can be used in URLs, even if the baseUrl is empty.
            pure $ "/" <> objectPath
        S3Storage { connectInfo, bucket, baseUrl } -> do
            let payload = fileInfo
                    |> (.fileContent)
                    |> Conduit.sourceLbs

            let contentType = cs (Wai.fileContentType fileInfo)
            contentDisposition <- (options.contentDisposition) fileInfo
            trySaveFile <- runMinio connectInfo do
                let options :: PutObjectOptions = defaultPutObjectOptions { pooContentType = Just contentType, pooContentDisposition = contentDisposition }
                putObject bucket objectPath payload Nothing options
            case trySaveFile of
                Left e -> throw e
                Right _ -> pure ()

            pure $ baseUrl <> objectPath

    pure StoredFile { path = objectPath, url }

-- | Fetches an url and uploads it to the storage.
--
-- The stored file has the content type provided by @Content-Type@ header of the downloaded file.
--
-- __Example:__ Copy a file from a remote server to the @pictures@ directory
--
-- > let externalUrl = "http://example/picture.jpg"
-- >
-- > let options :: StoreFileOptions = def
-- >         { directory = "pictures"
-- >         }
-- >
-- > storedFile <- storeFileFromUrl externalUrl options
-- > let newUrl = storedFile.url
--
storeFileFromUrl :: (?context :: context, ConfigProvider context) => Text -> StoreFileOptions -> IO StoredFile
storeFileFromUrl url options = do
    (contentType, responseBody) <- do
        response <- Wreq.get (cs url)
        let contentType = response ^. Wreq.responseHeader "Content-Type"
        let responseBody = response ^. Wreq.responseBody
        pure (contentType, responseBody)

    let file = Wai.FileInfo
            { fileName = ""
            , fileContentType = contentType
            , fileContent = responseBody
            }

    storeFileWithOptions file options


-- | Uploads a local file to the storage
--
-- The content type is guessed based on the file extension.
--
-- __Example:__ Copy a local "picture.jpg" to the @pictures@ directory inside the storage
--
-- >
-- > let options :: StoreFileOptions = def
-- >         { directory = "pictures"
-- >         }
-- >
-- > storedFile <- storeFileFromPath "picture.jpg" options
-- > let newUrl = storedFile.url
--
storeFileFromPath :: (?context :: context, ConfigProvider context) => Text -> StoreFileOptions -> IO StoredFile
storeFileFromPath path options = do
    let fileContentType = Mime.defaultMimeLookup (cs path)

    fileContent <- LBS.readFile (cs path)

    -- If fileName was passed (as UUID), use it. Otherwise, keep it empty, and a new random UUID will be generated.
    let fileName = case options.fileName of
            Just name -> cs $ UUID.toText name
            Nothing -> ""

    let file = Wai.FileInfo
            { fileName = fileName
            , fileContentType
            , fileContent
            }

    storeFileWithOptions file options

-- | Returns a signed url for a path inside the storage. The url is valid for 7 days.
--
-- If the 'StaticDirStorage' is used, a unsigned normal URL will be returned, as these files are public anyways.
--
-- __Example:__ Get a signed url for a path
--
-- >
-- > signedUrl <- createTemporaryDownloadUrlFromPath "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"
-- >
-- > let url :: Text = signedUrl.url
-- > let expiredAt :: UTCTime = signedUrl.expiredAt
--
-- See 'createTemporaryDownloadUrlFromPathWithExpiredAt' if you want to customize the url expiration time of 7 days.
--
createTemporaryDownloadUrlFromPath :: (?context :: context, ConfigProvider context) => Text -> IO TemporaryDownloadUrl
createTemporaryDownloadUrlFromPath objectPath = createTemporaryDownloadUrlFromPathWithExpiredAt (7 * 24 * 3600) objectPath


-- | Like 'createTemporaryDownloadUrlFromPath', but with a custom expiration time. Returns a signed url for a path inside the storage. The url is valid for 7 days.
--
-- If the 'StaticDirStorage' is used, a unsigned normal URL will be returned, as these files are public anyways.
--
-- __Example:__ Get a signed url for a path that expires in 5 minutes
--
-- > let validInSeconds = 5 * 60
-- > signedUrl <- createTemporaryDownloadUrlFromPathWithExpiredAt validInSeconds "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"
-- >
-- > let url :: Text = signedUrl.url
-- > let expiredAt :: UTCTime = signedUrl.expiredAt
--
createTemporaryDownloadUrlFromPathWithExpiredAt :: (?context :: context, ConfigProvider context) => Int -> Text -> IO TemporaryDownloadUrl
createTemporaryDownloadUrlFromPathWithExpiredAt validInSeconds objectPath = do
    publicUrlExpiredAt <- addUTCTime (fromIntegral validInSeconds) <$> getCurrentTime
    case storage of
        StaticDirStorage {} -> do
            let frameworkConfig = ?context.frameworkConfig
            let urlSchemes = ["http://", "https://"]

            let cleanPath = if "/" `isPrefixOf` objectPath
                    then Text.drop 1 objectPath
                    else objectPath

            let url = if any (`isPrefixOf` objectPath) urlSchemes
                    -- Legacy case: full URL saved, use as is.
                    then objectPath
                    -- Otherwise, construct full URL using baseUrl and cleaned path.
                    else frameworkConfig.baseUrl <> "/" <> cleanPath

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
-- > let url :: Text = signedUrl.url
-- > let expiredAt :: UTCTime = signedUrl.expiredAt
--
createTemporaryDownloadUrl :: (?context :: context, ConfigProvider context) => StoredFile -> IO TemporaryDownloadUrl
createTemporaryDownloadUrl storedFile = createTemporaryDownloadUrlFromPath (storedFile.path)

-- | Use the temporary download URL if the current one is not expired.
-- Otherwise, create a new temporary download URL and update the record.
--
-- __Example:__ Fetch an 'UploadedFile' record (a custom record with @signedUrl@, @signedUrlExpiredAt@ and @path@ ) and use 'refreshTemporaryDownloadUrlFromFile'
-- to get a fresh signed url if expired date has passed.
-- and update it with the signed url.
--
-- > uploadedFile <- fetch uploadedFileId
-- > uploadedFile <- refreshTemporaryDownloadUrlFromFile uploadedFile
refreshTemporaryDownloadUrlFromFile ::
    ( ?modelContext::ModelContext
    , ?context :: context
    , ConfigProvider context
    , CanUpdate record
    , HasField "signedUrl" record Text
    , HasField "signedUrlExpiredAt" record UTCTime
    , HasField "path" record Text
    , SetField "signedUrl" record Text
    , SetField "signedUrlExpiredAt" record UTCTime
    , SetField "path" record Text
    ) => record  -> IO record
refreshTemporaryDownloadUrlFromFile record = do
    now <- getCurrentTime
    let diff = diffUTCTime now record.signedUrlExpiredAt
    if diff > 0
        then do
            temporaryDownloadUrl <- createTemporaryDownloadUrlFromPath record.path
            record
                |> set #signedUrl (temporaryDownloadUrl |> get #url)
                |> set #signedUrlExpiredAt (temporaryDownloadUrl |> get #expiredAt)
                |> updateRecord

        else
            pure record

contentDispositionAttachmentAndFileName :: Wai.FileInfo LByteString -> IO (Maybe Text)
contentDispositionAttachmentAndFileName fileInfo = pure (Just ("attachment; filename=\"" <> cs (fileInfo.fileName) <> "\""))

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
uploadToStorageWithOptions :: forall (fieldName :: Symbol) record (tableName :: Symbol). (
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
        Just fileInfo -> do
            storeFileWithOptions fileInfo options { directory }
            |> Exception.try
            >>= \case
                Left (exception :: SomeException) -> record
                            |> attachFailure field ("Failed uploading to storage: " <> show exception)
                            |> pure
                Right storedFile -> record
                            |> setField @fieldName (Just (storedFile.url))
                            |> pure

        _ -> pure record

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
uploadToStorage :: forall (fieldName :: Symbol) record (tableName :: Symbol). (
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

-- | Permanently removes a previously stored file from storage.
--
-- __Example:__ Delete a previously uploaded file. The objectPath and url are stored in the database in this example.
--
-- > action DeleteUploadedFileAction { uploadedFileId } = do
-- >     uploadedFile <- fetch uploadedFile
-- >     let storedFile = StoredFile
-- >             { path = uploadedFile.objectPath
-- >             , url = uploadedFile.url
-- >             }
-- >     removeFileFromStorage storedFile
-- >     deleteRecord uploadedFile
-- >     redirectTo UploadedFilesAction
removeFileFromStorage :: (?context :: context, ConfigProvider context) => StoredFile -> IO (Either MinioErr ())
removeFileFromStorage StoredFile { path, url } = do
    case storage of
        StaticDirStorage { directory } -> do
            let fullPath :: String = cs $ directory <> path
            Directory.removeFile fullPath
            pure $ Right ()
        S3Storage { connectInfo, bucket} -> do
            runMinio connectInfo do
              removeObject bucket path

-- | Returns the current storage configured in Config.hs
storage :: (?context :: context, ConfigProvider context) => FileStorage
storage = ?context.frameworkConfig.appConfig
        |> TMap.lookup @FileStorage
        |> fromMaybe (error "Could not find FileStorage in config. Did you call initS3Storage from your Config.hs?")

-- | Returns the prefix for the storage. This is either @static/@ or an empty string depending on the storage.
storagePrefix :: (?context :: ControllerContext) => Text
storagePrefix = case storage of
    StaticDirStorage { directory } -> directory
    S3Storage { baseUrl} -> baseUrl
