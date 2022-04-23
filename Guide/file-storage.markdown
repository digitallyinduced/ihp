# File Storage & Uploads

```toc

```

## Introduction

A common task when building web applications is to save and manage uploaded files like custom logos, profile pictures or `.csv` files provided by the user.

IHP provides a simple file storage system to upload files to Amazon S3 or any S3 compatible cloud service.

When you're just starting out with IHP, we recommend you use the `static/` directory storage for now. When you move your project to production and things are getting more professional you can always switch to S3. Keep in mind: All files in the `static/` directory are typically publicly accessible.

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

You can use the [`uploadToStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorage) function to save a user upload to the storage:

```haskell
action UpdateCompanyAction { companyId } = do
    company <- fetch companyId
    company
        |> fill @'["name"]
        |> uploadToStorage #logoUrl
        >>= ifValid \case
            Left company -> render EditView { .. }
            Right company -> do
                company <- company |> updateRecord
                redirectTo EditCompanyAction { .. }
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

##### Custom File Field

If you need to more customization on the file field which the [`fileField`](https://ihp.digitallyinduced.com/api-docs/IHP-View-Form.html#v:fileField) helper doesn't allow, you can also use a handwritten file input:

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

    <input
        type="file"
        name="logoUrl"
        class="form-control-file"
        accept="image/*"
        data-preview="#logoUrlPreview"
    />

    <img id="logoUrlPreview"/>

    {submitButton}
|]
```


### Image Preprocessing

When dealing with images, we can use the imagemagick tool to e.g. resize the image and strip metadata:

This accepts any kind of image file compatible with ImageMagick, converts it to a 512x512 PNG and strip all meta data we use [`uploadToStorageWithOptions`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:uploadToStorageWithOptions) together with [`applyImageMagick`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Preprocessor-ImageMagick.html#v:applyImageMagick):

```haskell
action UpdateCompanyAction { companyId } = do
    let uploadLogo = uploadToStorageWithOptions $ def
            { preprocess = applyImageMagick "png" "-resize '512x512^' -gravity north -extent 512x512 -quality 100% -strip"  }

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
        { preprocess = applyImageMagick "jpg" "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"  }
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
    let url = get #url storedFile
```

This will upload the provided `<input type="file" name="logo"/>` to the `logos` directory. The [`storeFile`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFile) function returns [`StoredFile`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-Types.html#t:StoredFile) structure. We use `get #url` to read the url where the file was saved to.

There's also a [`storeFileWithOptions`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:storeFileWithOptions) to pass additional configuration:

```haskell
let file = fileOrNothing "file"
        |> fromMaybe (error "no file given")

let options :: StoreFileOptions = def
        { directory = "pictures"
        , preprocess = applyImageMagick "jpg" "-strip"
        }

storedFile <- storeFileWithOptions file options
let url = get #url storedFile
```

### Accessing Uploaded Files without Storing them

You can read the content of an uploaded file without saving it to the cloud storage. This can be useful when you're dealing with text files:

```haskell
action SubmitMarkdownAction = do
    let content :: Text =
            fileOrNothing "markdown"
            |> fromMaybe (error "no file given")
            |> get #fileContent
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

You can get the uploaded file name with `get #fileName file`

```haskell
action SubmitMarkdownAction = do
    let fileName :: Text =
            fileOrNothing "markdown"
            |> fromMaybe (error "no file given")
            |> get #fileName
            |> cs

    putStrLn fileName
```

## Signed Temporary Download Urls

When your S3 bucket is not configured for public read access, you need use a temporary download url to provide access to the file:

```haskell
signedUrl <- createTemporaryDownloadUrlFromPath "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"

let url :: Text = get #url signedUrl
let expiredAt :: UTCTime = get #expiredAt signedUrl
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
