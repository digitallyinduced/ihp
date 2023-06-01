# File Storage & Uploads

```toc

```

## Introduction

A common task when building web applications is to save and manage uploaded files like custom logos, profile pictures or `.csv` files provided by the user.

IHP provides a simple file storage system to upload files to Amazon S3 or any S3 compatible cloud service.

When you're just starting out with IHP, we recommend you use the `static/` directory storage for now. When you move your project to production and things are getting more professional you can always switch to S3. Keep in mind: All files in the `static/` directory are typically publicly accessible.

For security reasons all uploaded files get a random UUID as their file name. This makes it impossible to guess the URL of a file and also makes sure it's not possible to upload a file with a malicious file name.

## Configuration

### Static Directory

Open your `Config/Config.hs` and import `import IHP.FileStorage.Config`:

```haskell
import IHP.FileStorage.Config
```

Then add a call to [`initStaticDirStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Config.html#v:initStaticDirStorage):

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.FileStorage.Config

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initStaticDirStorage
```

### S3

Open your `Config/Config.hs` and import `import IHP.FileStorage.Config`:

```haskell
import IHP.FileStorage.Config
```

Then add a call to [`initS3Storage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Config.html#v:initS3Storage):

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.FileStorage.Config

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initS3Storage "eu-central-1" "my-bucket-name"
```

You need to replace `eu-central-1` with your availability zone and `my-bucket-name` with the name of your S3 bucket.

The AWS access key and secret key have to be provided using the `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` env vars.

For easy development you can add these env vars to your `./start` script:

```bash
#!/usr/bin/env bash
# Script to start the local dev server

# ...

export AWS_ACCESS_KEY_ID="YOUR KEY"            # <---------
export AWS_SECRET_ACCESS_KEY="YOUR SECRET"     # <---------

# Finally start the dev server
RunDevServer
```

### Minio

Open your `Config/Config.hs` and import `import IHP.FileStorage.Config`:

```haskell
import IHP.FileStorage.Config
```

Then add a call to [`initMinioStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Config.html#v:initMinioStorage):

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.FileStorage.Config

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initMinioStorage "https://minio.example.com" "my-bucket-name"
```

You need to replace `https://minio.example.com` with your minio server and `my-bucket-name` with the name of your bucket.

The Minio access key and secret key have to be provided using the `MINIO_ACCESS_KEY` and `MINIO_SECRET_KEY` env vars.

For easy development you can add these env vars to your `./start` script:

```bash
#!/usr/bin/env bash
# Script to start the local dev server

# ...

export MINIO_ACCESS_KEY="YOUR KEY"            # <---------
export MINIO_SECRET_KEY="YOUR SECRET"     # <---------

# Finally start the dev server
RunDevServer
```

### Filebase
> Filebase provides an S3 compatible API on top of decentralized object storage systems such as Storj, Skynet, Sia and IPFS

Open your `Config/Config.hs` and import `import IHP.FileStorage.Config`:

```haskell
import IHP.FileStorage.Config
```

Then add a call to [`initFilebaseStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Config.html#v:initFilebaseStorage):

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.FileStorage.Config

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initFilebaseStorage "my-bucket-name"
```

You need to replace `my-bucket-name` with the name of your bucket.

The Filebase access key and secret key have to be provided using the `FILEBASE_KEY` and `FILEBASE_SECRET` env vars.

For easy development you can add these env vars to your `./start` script:

```bash
#!/usr/bin/env bash
# Script to start the local dev server

# ...

export FILEBASE_KEY="YOUR KEY"            # <---------
export FILEBASE_SECRET="YOUR SECRET"     # <---------

# Finally start the dev server
RunDevServer
```

## Uploading

### Saving a User Upload to the Storage

In this example we assume the following data schema:

```sql
CREATE TABLE companies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    logo_url TEXT DEFAULT NULL
);
```

Note that the `logo_url` is a Maybe type, as the logo is optional. However, even if you wanted the logo to be required, you would still need to use a Maybe type, as the logo is not uploaded as part of the form submission. We will see soon how we can still ensure it's required.

You can use the [`uploadToStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorage) function to save a user upload to the storage:

```haskell
action UpdateCompanyAction { companyId } = do
    company <- fetch companyId
    company
        |> buildCompany
        |> uploadToStorage #logoUrl
        >>= ifValid \case
            Left company -> render EditView { .. }
            Right company -> do
                company <- company |> updateRecord
                redirectTo EditCompanyAction { .. }

buildCompany company = company
    |> fill @["name"]
    |> validateField #name nonEmpty
```

The call to [`uploadToStorage #logoUrl`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorage) will upload the file provided by the user. It will be saved as `companies/<some uuid>` on the configured storage. The the file url will be written to the `logoUrl` attribute.

After calling [`uploadToStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorage) inside a action, you typically need to use [`>>=`](https://ihp.digitallyinduced.com/api-docs/IHP-Prelude.html#v:-62--62--61-) instead of [`|>`](https://ihp.digitallyinduced.com/api-docs/IHP-HaskellSupport.html#v:-124--62-):

```haskell
-- Bad, will cause a type error:

company
    |> uploadToStorage #logoUrl
    |> someFunc
    |> ifValid \case

-- Good:

company
    |> uploadToStorage #logoUrl
    >>= someFunc
    >>= ifValid \case
```

#### Form

To submit a file upload, add a [`{fileField #logoUrl}`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:fileField) to the form that calls the action:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    {(fileField #logoUrl)}

    {submitButton}
|]
```

##### File Field Additional Attributes

If you need to more customization on the file field which the [`fileField`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:fileField) you can describe them under the `additionalAttributes` property:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    {(fileField #logoUrl) { additionalAttributes = [("accept", "image/*")] } }

    {submitButton}
|]
```

If for some reason you'd want to use hand written `input` you can still do that:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    <input
        type="file"
        name="logoUrl"
        class="form-control-file"
        accept="image/*"
    />

    {submitButton}
|]
```

It's important that `<input>` has `name="logoUrl` attribute, as that's where [`uploadToStorage #logoUrl`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorage) expects to find the file.

##### Inline Preview

For image uploads it's a good user experience to see a small preview of the uploaded file. IHP's `helpers.js` contain a small helper for this. You can set a `data-preview=".my-img-element"` attribute on the `<input type="file">` and a preview will be set on the element matched by the CSS selector in the `data-preview` attribute:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    {(fileField #logoUrl) { additionalAttributes = [("accept", "image/*"), ("data-preview", "#logoUrlPreview")] } }

    <img id="logoUrlPreview"/>

    {submitButton}
|]
```

On the "Edit.hs" file, it can be helpful to see the logo that has already been uploaded. To do this, change "<img id="logoUrlPreview"/>" to "<img id="logoUrlPreview" src={company.logoUrl}/>." This will allow the preview to show the existing logo, and also update to display any newly uploaded logos.

### Required Uploads

As notes above, the `logoUrl` must be a Maybe type, but there are cases where we want to ensure a file is uploaded as part of the record submission. We can add required to the form:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    {(fileField #logoUrl) { required = True }}

    {submitButton}
|]
```

That's handy, however frontend validation is enough, we need to do server side validation as well. We can do that by
slightly changing the order of our commands:

```haskell
action UpdateCompanyAction { companyId } = do
    company <- fetch companyId
    company
        -- Now we first upload the image, and populate the `logoUrl` field.
        |> uploadToStorage #logoUrl
        >>= buildCompany
        >>= ifValid \case
        -- ...


buildCompany company = company
    |> fill @["name"]
    |> validateField #name nonEmpty
    |> validateField #logoUrl nonEmpty -- Validate that the logoUrl is not empty.
    |> pure -- We're inside an IO, so we need to use `pure`

```

Note that `|> buildCompany` was changed to `>>= buildCompany`.

Now we have server side validation as well, and if a file isn't loaded, the user will see "The field cannot be empty". Your code however should still check that the `logoUrl` is not empty before using it.


### Image Preprocessing

When dealing with images, we can use the imagemagick tool to e.g. resize the image and strip metadata:

This accepts any kind of image file compatible with ImageMagick, converts it to a 512x512 PNG and strip all meta data we use [`uploadToStorageWithOptions`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorageWithOptions) together with [`applyImageMagick`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Preprocessor-ImageMagick.html#v:applyImageMagick):

```haskell
action UpdateCompanyAction { companyId } = do
    let uploadLogo = uploadToStorageWithOptions $ def
            { preprocess = applyImageMagick "png" ["-resize", "512x512^", "-gravity", "north", "-extent", "512x512", "-quality", "100%", "-strip"]  }

    company <- fetch companyId
    company
        |> fill @'["name"]
        |> uploadLogo #logoUrl
        >>= ifValid \case
            Left company -> render EditView { .. }
            Right company -> do
                company <- company |> updateRecord
                redirectTo EditCompanyAction { .. }
```

#### Installing ImageMagick

The [`applyImageMagick`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Preprocessor-ImageMagick.html#v:applyImageMagick) function requires `imagemagick` to be installed. You can install it by adding `imagemagick` to the `otherDeps` of your `default.nix`:

```nix
        otherDeps = p: with p; [
            imagemagick
        ];
```

After that run `make -B .envrc` and restart your development server.

#### Jpegs

To store an image as a jpeg and reduce its quality use this:

```haskell
let uploadLogo = uploadToStorageWithOptions $ def
        { preprocess = applyImageMagick "jpg" ["-resize", "1024x1024^", "-gravity", "north", "-extent", "1024x1024", "-quality", "85%", "-strip"] }
```

### Content Disposition

The browser uses the [`Content-Disposition`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition) header to detect if the file should be shown inside the browser or should be downloaded as a file attachment.

By default no `Content-Disposition` header is set.

If you use the S3 Storage you can use the [`contentDispositionAttachmentAndFileName`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:contentDispositionAttachmentAndFileName) function to mark a file as an attachment and use its original provided file name as the downloaded file name:

```haskell
let uploadAttachment = uploadToStorageWithOptions $ def
        { contentDisposition = contentDispositionAttachmentAndFileName  }

record
    |> uploadAttachment #attachmentUrl
```

This will render a header like this when requesting the file from S3:

```haskell
Content-Disposition: attachment; filename="the-filename-when-uploaded.pdf"
```

#### Custom Content-Disposition

You can provide your own function that generates the `Content-Disposition` header:

```haskell
makeContentDispositionHeader :: Wai.FileInfo LByteString -> IO (Maybe Text)
makeContentDispositionHeader fileInfo = do
    let header = "attachment; filename=download"
    pure (Just header)

let uploadAttachment = uploadToStorageWithOptions $ def
        { contentDisposition = makeContentDispositionHeader  }

record
    |> uploadAttachment #attachmentUrl
```

### Saving a User Upload without a Record

The above methods always require a record like `company` to store the files.

Use [`storeFile`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFile) to save uploads without a model:

```haskell
action UpdateLogoAction = do
    let file = fileOrNothing "logo"
            |> fromMaybe (error "No file given")

    storedFile <- storeFile file "logos"
    let url = storedFile.url
```

This will upload the provided `<input type="file" name="logo"/>` to the `logos` directory. The [`storeFile`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFile) function returns [`StoredFile`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Types.html#t:StoredFile) structure. We use `storedFile.url` to read the url where the file was saved to.

There's also a [`storeFileWithOptions`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFileWithOptions) to pass additional configuration:

```haskell
let file = fileOrNothing "file"
        |> fromMaybe (error "no file given")

let options :: StoreFileOptions = def
        { directory = "pictures"
        , preprocess = applyImageMagick "jpg" "-strip"
        }

storedFile <- storeFileWithOptions file options
let url = storedFile.url
```

There's also [`storeFileFromPath`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFileFromPath) to copy an existing file and [`storeFileFromUrl`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFileFromPath) to grab a file from a remote url.

When copying a file from path it's possible that you'd like to keep the same name of the original name. Note that
"original name" in this context means the UUID the file was saved in.

```haskell
import qualified Data.UUID as UUID

let file = fileOrNothing "file"
        |> fromMaybe (error "no file given")

-- Save the original file.
let options :: StoreFileOptions = def
        { directory = "pictures"
        }

storedFile <- storeFileWithOptions file options

-- Save a copy of the file as a thumbnail.
let options :: StoreFileOptions = def
        { directory = "pictures/thumbnails"
        -- Convert to a 100x100 thumbnail.
        , preprocess = applyImageMagick "jpg" ["-resize", "100x100^", "-gravity", "center", "-extent", "100x100", "-quality", "85%", "-strip"]
        , fileName = UUID.fromText $ cs file.fileName -- Convert the original file name to a UUID.
        }

storedFileThumbnail <- storeFileFromPath storedFile.path options
```


### Accessing Uploaded Files without Storing them

You can read the content of an uploaded file without saving it to the cloud storage. This can be useful when you're dealing with text files:

```haskell
action SubmitMarkdownAction = do
    let content :: Text =
            fileOrNothing "markdown"
            |> fromMaybe (error "no file given")
            |> (.fileContent)
            |> cs -- content is a LazyByteString, so we use `cs` to convert it to Text

    -- We can now do anything with the content of the uploaded file
    -- E.g. printing it to the terminal
    putStrLn content
```

To upload a file to this action you can use the following form:
```html
<form method="POST" action={SubmitMarkdownAction}>
    <input
        type="file"
        name="markdown"
        accept="text/markdown, text/plain"
    >
</form>
```

### Accessing Uploaded File Name

You can get the uploaded file name with `file.fileName`

```haskell
action SubmitMarkdownAction = do
    let fileName :: Text =
            fileOrNothing "markdown"
            |> fromMaybe (error "no file given")
            |> (.fileName)
            |> cs

    putStrLn fileName
```

## Signed Temporary Download Urls

When your S3 bucket is not configured for public read access, you need use a temporary download url to provide access to the file:

```haskell
signedUrl <- createTemporaryDownloadUrlFromPath "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"

let url :: Text = signedUrl.url
let expiredAt :: UTCTime = signedUrl.expiredAt
```

If the [`StaticDirStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Types.html#t:FileStorage) is used, a unsigned normal URL will be returned, as these files are public anyways.

The signed url is valid for 7 days.


## File Upload Limits

To avoid a single request overloading the server, [IHP has certain request limits in place](https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-Parse.html#v:defaultParseRequestBodyOptions):

- Maximum key/filename length: 32 bytes
- Maximum files: 10
- Filesize unlimited
- Maximum size for parameters: 64kbytes
- Maximum number of header lines: 32 bytes (applies only to headers of a mime/multipart message)
- Maximum header line length: 8190 (like apache)

You can change the limits in the `Config/Config.hs` like this:

```haskell
import qualified Network.Wai.Parse as WaiParse -- <--- ADD THIS IMPORT

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    -- We extend the default options here
    option $ WaiParse.defaultParseRequestBodyOptions
            |> WaiParse.setMaxRequestNumFiles 20 -- Increase count of allowed files per request

```

[You can find a full list of `WaiParser.set...` functions on the `Network.Wai.Parse` documentation](https://hackage.haskell.org/package/wai-extra-3.1.6/docs/Network-Wai-Parse.html#v:setMaxRequestKeyLength)

## Image Style Implementation

Image style is a common feature in web applications. It allows you to define different styles for an image, like a thumbnail or a preview image. The image style is then applied to the image when it's viewed. One image can have multiple image styles. So on one page it would appear as a thumbnail, on another page it would appear with a 400px x 200px dimension. The original image remains as is, untouched, and whenever there is a demand for an image style then it is generated on the fly. If an image style was already generated then it is served, saving the effort of re-generating it.

Let's see how we can implement that. At first, without any access control, only accept the original image along with the desired dimensions and generate the image style on the fly.

We add an action to the `ImageStyleController` that accepts the original image path, the desired width and height.

```haskell
-- Web/Types.hs
-- ...
data ImageStyleController
    = RenderImageStyleAction { width :: !Int, height :: !Int, originalImagePath :: !Text}
    deriving (Eq, Show, Data)
```

Wire the new route.

```haskell
-- Web/Routes.hs
-- ...
instance AutoRoute ImageStyleController
```

And add it to the `FrontController`

```haskell
-- Web/FrontController.hs

-- Controller Imports
-- ...
import Web.Controller.ImageStyle

instance FrontController WebApplication where
    controllers =
        [ startPage StartPageAction
        -- Generator Marker
        , parseRoute @ImageStyleController
        -- ...
        ]
```

Then we have our controller logic.

```haskell
-- Web/Controller/ImageStyle.hs

module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)
import qualified Data.Text as Text


instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, originalImagePath } = do
        -- Get the original image directory and UUID from the path.
        let (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath

        let size = show width <> "x" <> show height
        let imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
        let imageStylePath = imageStylePathDirectory <> "/" <> uuid

        -- If we use a StaticDirStorage storage then we need to prefix the path with the `static/` folder.
        let storagePrefix = case storage of
                StaticDirStorage -> "static/"
                _ -> ""

        fileExists <- doesFileExist (cs $ storagePrefix <> imageStylePath)

        if fileExists
            then do
                -- Image style found.
                renderFile (cs $ storagePrefix <> imageStylePath) "application/jpg"
            else do
                -- Image style not found, so create it.
                let options :: StoreFileOptions = def
                        { directory = imageStylePathDirectory
                        , preprocess = applyImageMagick "jpg" ["-resize", cs size <> "^", "-gravity", "center", "-extent", cs size, "-quality", "85%", "-strip"]
                        }

                storedFile <- storeFileFromPath (cs $ storagePrefix <> originalImageDirectory <> "/" <> uuid) options

                renderFile (cs $ storagePrefix <> storedFile.path) "application/jpg"

-- | Extracts the directory and UUID from a path like "pictures/8ed22caa-11ea-4c45-a05e-91a51e72558d"
extractDirectoryAndUUID :: (?context :: context, ConfigProvider context) => Text -> (Text, Text)
extractDirectoryAndUUID inputText =
    case reverse parts of
        uuid : pathSegments -> (Text.intercalate "/" (reverse pathSegments), uuid)
        _ -> ("", "")
    where
        frameworkConfig = ?context.frameworkConfig
        trimmedText = Text.replace (frameworkConfig.baseUrl <> "/") "" inputText
        parts = Text.splitOn "/" trimmedText
```

Now, from any `Show` action, we can use the image style. Here we create a 400px x 200px image style for the original image.

```haskell
[hsx|<img src={pathTo $ RenderImageStyleAction 400 200 imageUrl} />|]
where
    imageUrl = "http://localhost:8000/static/picture/b4c8f55c-16d6-41f0-9503-77352b134e14"
```

### Security Considerations

The above implementation is not secure. Anyone can request any image style of any image. This may put a burden on the site. Imagine a malicious user requesting thousands of different dimensions of a 100MB image. The server would have to generate thousands of images. This would put a lot of load on the server. To prevent this, we need to add some access control. The way we would do it, as that is the simplest way, is to add a signed token to the image style URL. The token would be generated by the server and would contain the original image path, the desired width and height. The server would then verify the token and only serve the image style if the token is valid. This way, the server would not have to generate the image style on the fly, but only if the token is valid. If the token is not valid, then the server would return an access denied.

We start by adding the `cryptonite` package to the `default.nix` file, and re-start `./start` to get the package.

```nix
# default.nix
haskellEnv = import "${ihp}/NixSupport/default.nix" {
    ihp = ihp;
    haskellDeps = p: with p; [
        cabal-install
        base
        # ...
        cryptonite # <--- ADD THIS
    ];
```

Then we need to generate a private and public key pair. We can do it in our Config file, and save the keys for later use.

```haskell
-- Config/Config.hs
import Crypto.PubKey.RSA as RSA

newtype RsaPublicAndPrivateKeys = RsaPublicAndPrivateKeys (RSA.PublicKey, RSA.PrivateKey)

config :: ConfigBuilder
config = do
    -- See https://ihp.digitallyinduced.com/Guide/config.html
    -- for what you can do here

    -- ...

    -- Private and public keys to sign and verify image style URLs.
    -- The 300 means how large your URL can be.
    -- The 65537 is the public exponent of RSA.
    -- You can probably leave those values as is.
    (publicKey, privateKey) <- liftIO $ liftIO $ RSA.generate 300 65537
    option $ RsaPublicAndPrivateKeys (publicKey, privateKey)
```

Your Controller will now look like this:

```haskell
module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)
import qualified Data.Text as Text

-- Imports for the signed token.
import Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Hash.Algorithms as Hash.Algorithms
import Config
import Data.ByteString.Base64 as Base64

instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, originalImagePath, signed } = do
        let size = show width <> "x" <> show height

        -- Verify the signed token.
        let Config.RsaPublicAndPrivateKeys (publicKey, _) = getAppConfig @Config.RsaPublicAndPrivateKeys

        -- Verify the token, and deny or allow access based on the result.
        accessDeniedUnless case cs signed |> Base64.decode of
            Left msg -> False
            Right signed -> RSA.verify (Just Hash.Algorithms.SHA256) publicKey (cs $ originalImagePath <> size) signed

        -- Get the original image directory and UUID from the path.
        let (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath


        let imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
        let imageStylePath = imageStylePathDirectory <> "/" <> uuid

        -- If we use a StaticDirStorage storage then we need to prefix the path with the `static/` folder.
        let storagePrefix = case storage of
                StaticDirStorage -> "static/"
                _ -> ""

        fileExists <- doesFileExist (cs $ storagePrefix <> imageStylePath)

        if fileExists
            then do
                -- Image style found.
                renderFile (cs $ storagePrefix <> imageStylePath) "application/jpg"
            else do
                -- Image style not found, so create it.
                let options :: StoreFileOptions = def
                        { directory = imageStylePathDirectory
                        , preprocess = applyImageMagick "jpg" ["-resize", cs size <> "^", "-gravity", "center", "-extent", cs size, "-quality", "85%", "-strip"]
                        }

                storedFile <- storeFileFromPath (cs $ storagePrefix <> originalImageDirectory <> "/" <> uuid) options

                renderFile (cs $ storagePrefix <> storedFile.path) "application/jpg"

-- | Extracts the directory and UUID from a path like "pictures/8ed22caa-11ea-4c45-a05e-91a51e72558d"
extractDirectoryAndUUID :: (?context :: context, ConfigProvider context) => Text -> (Text, Text)
extractDirectoryAndUUID inputText =
    case reverse parts of
        uuid : pathSegments -> (Text.intercalate "/" (reverse pathSegments), uuid)
        _ -> ("", "")
    where
        frameworkConfig = ?context.frameworkConfig
        trimmedText = Text.replace (frameworkConfig.baseUrl <> "/") "" inputText
        parts = Text.splitOn "/" trimmedText
```

We need to change `RenderImageStyleAction` to get another argument, the signed token.


```haskell
-- Web/Types.hs
-- ...
data ImageStyleController
    = = RenderImageStyleAction { width :: !Int, height :: !Int, originalImagePath :: !Text, signed :: !Text }
    deriving (Eq, Show, Data)
```

Now, our `Show` action is responsible for generating the signed token. We can do it like this:

```haskell
[hsx|<img src={pathTo $ RenderImageStyleAction 400 200 imageUrl signed} />|]
where
    imageUrl = "http://localhost:8000/static/picture/b4c8f55c-16d6-41f0-9503-77352b134e14"

    -- Sign the image URL to prevent tampering.
    Config.RsaPublicAndPrivateKeys (_, privateKey) = getAppConfig @Config.RsaPublicAndPrivateKeys
    signed = case RSA.sign Nothing (Just Hash.Algorithms.SHA256) privateKey (cs $ imageUrl <> "400x200") of
        Left msg -> error $ "Cannot sign image URL, private key is invalid:" <> show msg
        -- Base 64 decode the token so that it can be used in the URL.
        -- and use `cs` to convert it to `Text`.
        Right signature -> signature |> Base64.encode |> cs
```