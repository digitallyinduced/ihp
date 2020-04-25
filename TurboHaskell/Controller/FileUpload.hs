{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType, BlockArguments, DataKinds #-}

{-|
Module: TurboHaskell.Controller.FileUpload
Description: Easy access to uploaded files
Copyright: (c) digitally induced GmbH, 2020
-}
module TurboHaskell.Controller.FileUpload where

import TurboHaskell.Prelude

import Network.Wai.Parse (FileInfo, fileContent)
import qualified TurboHaskell.ModelSupport as ModelSupport
import qualified Data.ByteString.Lazy as LBS
import qualified System.Process as Process
import TurboHaskell.Controller.RequestContext
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified System.Process as Process

{-# INLINE fileOrNothing #-}
fileOrNothing :: (?requestContext :: RequestContext) => ByteString -> Maybe (FileInfo LBS.ByteString)
fileOrNothing !name = ?requestContext |> getField @"files" |> lookup name

data ImageUploadOptions = ImageUploadOptions { convertTo :: Text, imageMagickOptions :: Text }

-- | Accepts a uploaded png file
--
-- Example:
--
-- > user |> uploadPng #profilePicture`
uploadPng :: _ => Proxy fieldName -> record -> IO record
uploadPng field record = uploadImageFile "png" field record

uploadSVG :: _ => Proxy fieldName -> record -> IO record
uploadSVG = uploadImageFile "svg"

uploadImageWithOptions :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?requestContext :: RequestContext
        , ?modelContext :: ModelSupport.ModelContext
        , ?controllerContext :: context
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
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

uploadImageFile :: forall (fieldName :: Symbol) context record (tableName :: Symbol). (
        ?requestContext :: RequestContext
        , ?modelContext :: ModelSupport.ModelContext
        , ?controllerContext :: context
        , SetField fieldName record (Maybe Text)
        , KnownSymbol fieldName
        , HasField "id" record (ModelSupport.Id (ModelSupport.NormalizeModel record))
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
                |> return
        _ -> pure user

