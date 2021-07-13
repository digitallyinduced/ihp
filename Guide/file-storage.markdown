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

Then add a call to `initStaticDirStorage`:

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

Then add a call to `initS3Storage`:

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

You need to replace `eu-central-1` with your availbility zone and `my-bucket-name` with the name of your S3 bucket.

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

Then add a call to `initMinioStorage`:

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


## Uploading

### Saving a User Upload to the Storage

In this example we asume the following data schema:

```sql
CREATE TABLE companies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    logo_url TEXT DEFAULT NULL
);
```

You can use the `uploadToStorage` function to save a user upload to the storage:

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

The call to `uploadToStorage #logoUrl` will upload the file provided by the user. It will be saved as `companies/<some uuid>` on the configured storage. The the file url will be written to the `logoUrl` attribute.

After calling `uploadToStorage` inside a action, you typically need to use `>>=` instead of `|>`:

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

To submit a file upload, add a `<input type="file"/>` to the form that calls the action:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    <input type="file" name="logoUrl" class="form-control form-control-file"/>

    {submitButton}
|]
```

It's important that `<input>` has `name="logoUrl` attribute, as that's where `uploadToStorage #logoUrl` expects to find the file.

### Image Preprocessing

When dealing with images, we can use the imagemagick tool to e.g. resize the image and strip metadata:

This accepts any kind of image file compatible with ImageMagick, converts it to a 512x512 PNG and strip all meta data we use `uploadToStorageWithOptions` together with `applyImageMagick`:

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

The `applyImageMagick` function requires `imagemagick` to be installed. You can install it by adding `imagemagick` to the `otherDeps` of your `default.nix`:

```nix
        otherDeps = p: with p; [
            imagemagick
        ];
```

After that run `make -B .envrc` and restart your development server.


#### Jpegs

To store an image as a jpeg and reduce it's quality use this:

```haskell
let uploadLogo = uploadToStorageWithOptions $ def
        { preprocess = applyImageMagick "jpg" "-resize '1024x1024^' -gravity north -extent 1024x1024 -quality 85% -strip"  }
```

### Content Disposition

The browser uses the [`Content-Disposition`](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Disposition) header to detect if the file should be shown inside the browser or should be downloaded as a file attachment.

By default no `Content-Disposition` header is set.

If you use the S3 Storage you can use the `contentDispositionAttachmentAndFileName` function to mark a file as an attachment and use it's original provided file name as the downloaded file name:


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

Use `storeFile` to save uploads without a model:

```haskell
action UpdateLogoAction = do
    let file = fileOrNothing "logo"
            |> fromMaybe (error "No file given")

    storedFile <- storeFile file "logos"
    let url = get #url storedFile
```

This will upload the provided `<input type="file" name="logo"/>` to the `logos` directory. The `storeFile` function returns `StoredFile` structure. We use `get #url` to read the url where the file was saved to.

There's also a `storeFileWithOptions` to pass additional configuration:

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


## Signed Temporary Download Urls

When your S3 bucket is not configured for public read access, you need use a temporary download url to provide access to the file:

```haskell
signedUrl <- createTemporaryDownloadUrlFromPath "logos/8ed22caa-11ea-4c45-a05e-91a51e72558d"

let url :: Text = get #url signedUrl
let expiredAt :: UTCTime = get #expiredAt signedUrl
```

If the `StaticDirStorage` is used, a unsigned normal URL will be returned, as these files are public anyways.

The signed url is valid for 7 days.
