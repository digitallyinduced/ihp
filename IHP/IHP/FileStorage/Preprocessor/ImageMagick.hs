{-|
Module: IHP.FileStorage.Preprocessor.ImageMagick
Description: Preprocessor for images. Requires that you add @imagemagick@ to the @otherDeps@ inside the project's @default.nix.
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.FileStorage.Preprocessor.ImageMagick
( applyImageMagick
) where

import IHP.Prelude

import qualified Network.Wai.Parse as Wai
import qualified Data.ByteString.Lazy as LBS
import qualified System.IO.Temp as Temp
import qualified System.Process as Process


-- | Converts the image to the specified output format and applies specified image magick transforms
--
-- __Example:__ Transform an uploaded image to a PNG file, resize it to 512x512 and strip meta data
--
-- > let uploadLogo = uploadToStorageWithOptions $ def
-- >         { preprocess = applyImageMagick "png" ["-resize", "512x512^", "-gravity", "north", "-extent" , "512x512", "-quality", "100%", "-strip"] }
-- >
-- > company <- fetch companyId
-- > company
-- >     |> fill @'["name"]
-- >     |> uploadLogo #logoUrl
-- >     >>= ifValid \case
-- >         Left company -> render EditView { .. }
-- >         Right company -> do
-- >             company <- company |> updateRecord
-- >             redirectTo EditCompanyAction { .. }
--
--
-- __Example:__ Transform an uploaded image to a JPEG file and strip meta data
--
-- > let uploadLogo = uploadToStorageWithOptions $ def
-- >         { preprocess = applyImageMagick "jpg" ["-strip"] }
-- >
-- > company <- fetch companyId
-- > company
-- >     |> fill @'["name"]
-- >     |> uploadLogo #logoUrl
-- >     >>= ifValid \case
-- >         Left company -> render EditView { .. }
-- >         Right company -> do
-- >             company <- company |> updateRecord
-- >             redirectTo EditCompanyAction { .. }
--
applyImageMagick :: Text -> [String] -> Wai.FileInfo LByteString -> IO (Wai.FileInfo LByteString)
applyImageMagick convertTo otherParams fileInfo = do
    Temp.withTempDirectory "/tmp" "ihp-upload" $ \tempPath -> do
        let tempFilePath = tempPath <> "/image"
        let convertedFilePath = tempPath <> "/converted." <> cs convertTo

        fileInfo
            |> get #fileContent
            |> LBS.writeFile tempFilePath

        let params :: [String] = [tempFilePath] <> otherParams <> [convertedFilePath]
        Process.callProcess "convert" params

        newContent <- LBS.readFile convertedFilePath
        pure fileInfo { Wai.fileContent = newContent }