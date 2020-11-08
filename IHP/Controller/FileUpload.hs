{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType, BlockArguments, DataKinds #-}

{-|
Module: IHP.Controller.FileUpload
Description: Easy access to uploaded files
Copyright: (c) digitally induced GmbH, 2020

This modules provides high-level file and image upload functionality.

All uploaded files are saved to the `uploads` directory. Given e.g. an User entity with @id = 550e8400-e29b-11d4-a716-446655440000@, the file is saved to @\/uploads\/users\/550e8400-e29b-11d4-a716-446655440000\/picture.jpg@. If the directory does not exists, it will be created.

-}
module IHP.Controller.FileUpload where

import IHP.Prelude

import Network.Wai.Parse (FileInfo, fileContent)
import qualified IHP.ModelSupport as ModelSupport
import qualified Data.ByteString.Lazy as LBS
import qualified System.Process as Process
import IHP.Controller.RequestContext
import IHP.Controller.Context
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified System.Process as Process

-- | Returns a file upload from the request as a ByteString.
--
-- Returns `Nothing` when the file is not found in the request body.
fileOrNothing :: (?context :: ControllerContext) => ByteString -> Maybe (FileInfo LBS.ByteString)
fileOrNothing !name =
        ?context
        |> get #requestContext
        |> get #requestBody
        |> \case
            FormBody { files } -> lookup name files
            _ -> Nothing
{-# INLINE fileOrNothing #-}

-- | Options to be used together with 'uploadImageWithOptions'
--
-- __Example:__
-- 
-- > ImageUploadOptions { convertTo = "jpg", imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip" }
data ImageUploadOptions = ImageUploadOptions {
    -- | The file extension to be used when saving the file, e.g. @"jpg"@ or @"png"@.
      convertTo :: Text
    -- | Command line options passed to imagemagick. Can used for e.g. resizing, rotating, file size reduction.
    , imageMagickOptions :: Text
    }

-- | Saves an uploaded image file to the @uploads@ directory and writes the relative path to the given record attribute.
--
-- Given e.g. an User entity with @id = 550e8400-e29b-11d4-a716-446655440000@, the file is saved to @\/uploads\/users\/550e8400-e29b-11d4-a716-446655440000\/picture.jpg@.
--
-- Before saving, the image is converted using imagemagick. You can supply custom image magick options using the options attribute.
--
-- If the upload directory does not exists, it will be created.
--
-- __Example:__ Uploading a user profile picture
--
-- > let profilePictureOptions = ImageUploadOptions
-- >         { convertTo = "jpg"
-- >         , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
-- >         }
-- >
-- > user
-- >     |> fill @["firstname", "lastname", "pictureUrl"]
-- >     |> uploadImageWithOptions profilePictureOptions #pictureUrl
-- >     >>= ifValid \case
-- >         Left user -> render EditView { .. }
-- >         Right user -> do
-- >             user <- user |> updateRecord
-- >             redirectTo EditUserAction { .. }
--
-- The uploaded image path is now stored in #pictureUrl.
uploadImageWithOptions :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?context :: ControllerContext
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , Show (ModelSupport.PrimaryKey (ModelSupport.GetTableName (ModelSupport.NormalizeModel record)))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
    ) => ImageUploadOptions -> Proxy fieldName -> record -> IO record
uploadImageWithOptions options _ user =
    let
        fieldName :: ByteString = cs (symbolVal (Proxy @fieldName))
        tableName :: Text = cs (symbolVal (Proxy @tableName))
        uploadDir :: Text = "static"
        baseImagePath :: Text = "/uploads/" <> tableName <> "/" <> tshow (getField @"id" user) <> "/picture."
        imagePath :: Text = baseImagePath <> "jpg"
        uploadFilePath = baseImagePath <> "upload"
    in case fileOrNothing fieldName of
        Just file | fileContent file /= "" -> liftIO do
            _ <- Process.system ("mkdir -p `dirname " <> cs (uploadDir <> uploadFilePath) <> "`")
            let fullImagePath = uploadDir <> imagePath
            (fileContent file) |> LBS.writeFile (cs (uploadDir <> uploadFilePath))
            Process.runCommand (cs ("convert " <> cs uploadDir <> uploadFilePath <> " " <> (getField @"imageMagickOptions" options) <> " " <> cs fullImagePath))
            user
                |> setField @fieldName (Just (cs imagePath :: Text))
                |> return
        _ -> pure user

-- | Saves an uploaded image file to the `uploads` directory.
--
-- Given e.g. an User entity with @id = 550e8400-e29b-11d4-a716-446655440000@, the file is saved to @\/uploads\/users\/550e8400-e29b-11d4-a716-446655440000\/picture.jpg@.
--
-- No transformation or validation is applied to the given uploaded file. If you need this, take a look at 'uploadImageWithOptions'.
--
-- __Example:__ Uploading a user profile picture
--
-- > let profilePictureOptions = ImageUploadOptions
-- >         { convertTo = "jpg"
-- >         , imageMagickOptions = "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"
-- >         }
-- >
-- > user
-- >     |> fill @["firstname", "lastname", "pictureUrl"]
-- >     |> uploadImageFile "png" #pictureUrl
-- >     >>= ifValid \case
-- >         Left user -> render EditView { .. }
-- >         Right user -> do
-- >             user <- user |> updateRecord
-- >             redirectTo EditUserAction { .. }
--
uploadImageFile :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?context :: ControllerContext
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
        , Show (ModelSupport.PrimaryKey (ModelSupport.GetTableName (ModelSupport.NormalizeModel record)))
        , tableName ~ ModelSupport.GetTableName record
        , KnownSymbol tableName
    ) => Text -> Proxy fieldName -> record -> IO record
uploadImageFile ext _ user =
    let
        fieldName :: ByteString = cs (symbolVal (Proxy @fieldName))
        tableName :: Text = cs (symbolVal (Proxy @tableName))
        uploadDir :: Text = "static"
        imagePath :: Text = "/uploads/" <> tableName <> "/" <> tshow (getField @"id" user) <> "/picture." <> ext
    in case fileOrNothing fieldName of
        Just file | fileContent file /= "" -> liftIO do
            _ <- Process.system ("mkdir -p `dirname " <> cs (uploadDir <> imagePath) <> "`")
            (fileContent file) |> LBS.writeFile (cs $ uploadDir <> imagePath)
            user
                |> setField @fieldName (Just (cs imagePath :: Text))
                |> pure
        _ -> pure user

-- | Saves an uploaded png file. No validation or transformation applied.
-- See 'uploadImageFile' for details.
uploadPng ::
    ( ?context :: ControllerContext
    , SetField fieldName record (Maybe Text)
    , HasField "id" record (ModelSupport.Id' (GetTableName (ModelSupport.GetModelByTableName (GetTableName record))))
    , Show (ModelSupport.PrimaryKey (GetTableName (ModelSupport.GetModelByTableName (GetTableName record))))
    , KnownSymbol fieldName
    , KnownSymbol (GetTableName record)
    ) => Proxy fieldName -> record -> IO record
uploadPng field record = uploadImageFile "png" field record

-- | Saves an uploaded svg file. No validation or transformation applied.
-- See 'uploadImageFile' for details.
uploadSVG ::
    ( ?context :: ControllerContext
    , SetField fieldName record (Maybe Text)
    , HasField "id" record (ModelSupport.Id' (GetTableName (ModelSupport.GetModelByTableName (GetTableName record))))
    , Show (ModelSupport.PrimaryKey (GetTableName (ModelSupport.GetModelByTableName (GetTableName record))))
    , KnownSymbol fieldName
    , KnownSymbol (GetTableName record)
    ) => Proxy fieldName -> record -> IO record
uploadSVG = uploadImageFile "svg"
