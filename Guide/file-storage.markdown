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

AWS S3 is a popular cloud storage service that allows you to store and retrieve files. IHP provides a simple way to integrate with S3.
See MinIo section below to learn how to setup an S3 compatible storage service for your local development. So your application can use the same code for
both local development and production.

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

For easy development you can add these env vars to your `.envrc` file:

```bash
source_url "https://raw.githubusercontent.com/cachix/devenv/d1f7b48e35e6dee421cfd0f51481d17f77586997/direnvrc" "sha256-YBzqskFZxmNb3kYVoKD9ZixoPXJh1C9ZvTLGFRkauZ0="

use devenv

export AWS_ACCESS_KEY_ID="YOUR KEY"            # <---------
export AWS_SECRET_ACCESS_KEY="YOUR SECRET"     # <---------
```

### Minio

Enable MinIo in your `flake.nix`

```nix
devenv.shells.default = {
    # Enable S3 compatibility with MinIO.
    services.minio = {
        enable = true;
        buckets = [ "ihp-bucket" ];
    };

};
```

When working locally, the MinIO server is started by the `devenv up` command.

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

    -- Local development, we use MinIo.
    initMinioStorage "http://127.0.0.1:9000" "ihp-bucket"

    -- Or if you have a remote Minio server:
    -- initMinioStorage "https://minio.example.com" "my-bucket-name"
```

The Minio access key and secret key have to be provided using the `MINIO_ACCESS_KEY` and `MINIO_SECRET_KEY` env vars.

For easy development you can add these env vars to your `.envrc` file:

```bash
source_url "https://raw.githubusercontent.com/cachix/devenv/d1f7b48e35e6dee421cfd0f51481d17f77586997/direnvrc" "sha256-YBzqskFZxmNb3kYVoKD9ZixoPXJh1C9ZvTLGFRkauZ0="

use devenv

# ...
# When working locally, these are the default credentials.
export MINIO_ACCESS_KEY="minioadmin"
export MINIO_SECRET_KEY="minioadmin"
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

For easy development you can add these env vars to your `.envrc` file:

```bash
source_url "https://raw.githubusercontent.com/cachix/devenv/d1f7b48e35e6dee421cfd0f51481d17f77586997/direnvrc" "sha256-YBzqskFZxmNb3kYVoKD9ZixoPXJh1C9ZvTLGFRkauZ0="

use devenv

export FILEBASE_KEY="YOUR KEY"            # <---------
export FILEBASE_SECRET="YOUR SECRET"     # <---------
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

#### Remove an uploaded image

It's a good UX for optional image fields to allow the user to remove a previously uploaded image. To do this, we can add a checkbox to the form with the sole purpose of removing the image:

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    <div class="file-upload-wrapper">
        {(fileField #logoUrl) { additionalAttributes = [("accept", "image/*"), ("data-preview", "#logoUrlPreview")] } }

        <img id="logoUrlPreview"/>

        <div class={removeFileWrapperClasses}>
            {- Add a checbkox to allow removing the image -}
            <input type="checkbox" class="remove-file-checkbox" name="logoUrlRemove">
            <label for="logoUrlRemove">Remove image</label>
        </div>
    </div>

    {submitButton}
|]
    where
        removeFileWrapperClasses =
            classes
                [ "remove-file-wrapper"
                , ("hidden", isNothing company.logoUrl)
                ]
```

Let's add `imageOrEmptyImage` in a centralized place, so we can use it in other views:

```haskell
-- Application/Helper/View.hs

-- | We don't set the image `src` to null, so we we don't get a broken image icon.
-- Instead we set it to an empty image, from https://stackoverflow.com/a/9967193/750039
imageOrEmptyImage :: Maybe Text -> Text
imageOrEmptyImage (Just url) = url
imageOrEmptyImage Nothing = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="
```

Above code assumes you have `hidden` class, as you have when using TailwindCSS. If you use Bootstrap you can use `d-none`. If you don't have that class, you can use the following CSS:

```css
# app.css

.hidden {
    display: none;
}
```

Next we'd like our controller to listen to the new "Remove image" checkbox and act accordingly.

```haskell
-- Web/Controller/Companies.hs
action UpdateCompanyAction { .. } = do
    company <- fetch companyId

    -- The checkbox to "Remove image".
    let imageUrlRemove = paramOrDefault False "logoUrlRemove"
    user
        |> buildCompany
        >>= uploadOrRemoveMaybeImage #logoUrl logoUrlRemove
        >>= ifValid \case
            Left company -> do
                -- ...
```

Let's add `uploadOrRemoveMaybeImage` to centralized place, so other controllers can use it as well:

```haskell
-- Application/Helper/Controller.hs


-- | Uploads the image to the storage, if the image is present.
-- If the checkbox to remove the file was checked, then we remove the file
-- reference from the record.
uploadOrRemoveMaybeImage ::
    ( ?context::ControllerContext
    , Show (PrimaryKey (GetTableName (GetModelByTableName (GetTableName record))))
    , KnownSymbol fieldName, KnownSymbol (GetTableName record)
    , HasField "id" record (Id' (GetTableName (GetModelByTableName (GetTableName record))))
    , HasField "meta" record MetaBag, SetField fieldName record (Maybe Text)
    , SetField "meta" record MetaBag
    ) => Proxy fieldName -> Bool -> record -> IO record
uploadOrRemoveMaybeImage imageProperty isRemove record = do
    if isRemove
        then pure $ record |> set imageProperty Nothing
        else uploadToStorage imageProperty record
```

Note that the above function doesn't actually remove the file from the storage. It just removes the reference to the file from the record. To remove the file from storage you would need to call [`removeFileFromStorage`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:removeFileFromStorage).

Finally, let's add some Javacript, that would take care of removing the image from the file input, as well as hiding the "Remove image" checkbox when there's no image to remove.

```javascript
# app.js

$(document).on('ready turbolinks:load', () => {

    // Show the remove file checkbox if a file is uploaded.
    $('form .file-upload-wrapper :input').on('change', function() {
        const $this = $(this);

        $(this).closest('.file-upload-wrapper')
            .find('.remove-file-wrapper')
            .toggleClass('hidden', !$this.val())
            // Uncheck the "Remove file" checkbox, in case it was previously checked.
            .find(':checkbox')
            .prop('checked', false);
    });

    // Hide the file upload wrapper if the remove file checkbox is checked.
    $('form .remove-file-checkbox:checkbox').on('change', function() {
        const $wrapper = $(this).closest('.file-upload-wrapper');
        $wrapper
            .find(':file')
            .val(null);

        $wrapper
            .find('img')
            // We don't null the src, so we we don't get a broken image icon.
            // from https://stackoverflow.com/a/9967193/750039
            .attr('src', 'data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==');

        // Hide the remove file checkbox.
        $wrapper.find('.remove-file-wrapper')
            .addClass('hidden');
    });
});
```

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

That's handy, however, frontend validation is not enough, we need to do server-side validation as well. We can do that by
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

### Required Uploads with a File Record

We've just seen how we we're able to validate that the `logoUrl` is required, even though we've set `logoUrl` to be a `Maybe Text`. But what if the logo is indeed required? Modeling it as `Maybe Text` just because `uploadToStorage` returns a `Maybe` value isn't ideal. Just by looking at the model, another developer might, rightfully, assume that the logo is optional.

There's another things that's worth mentioning here, and that's the fact the file names are gone once we upload the file. For security reasons we don't want to use the original file name provided by the user. Instead we use a random UUID as the file name. This makes it impossible to guess the URL of a file and also makes sure it's not possible to upload a file with a malicious file name. But what if we do need the file name?

To solve both problems we can introduce a new model `UploadedFile`:

```sql
CREATE TABLE uploaded_files (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    signed_url TEXT NOT NULL,
    signed_url_expired_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    path TEXT NOT NULL,
    file_name TEXT NOT NULL,
    content_type TEXT NOT NULL
);
```

This will hold the original file name, the content type and the path where the file was saved. We also store a signed URL that can be used to download the file.

Companies will now reference this new record

```sql
CREATE TABLE companies (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    uploaded_file_id UUID NOT NULL
);
CREATE INDEX companies_uploaded_file_id_index ON companies (uploaded_file_id);
ALTER TABLE companies ADD CONSTRAINT companies_ref_uploaded_file_id FOREIGN KEY (uploaded_file_id) REFERENCES uploaded_files (id) ON DELETE NO ACTION;
```

Our Companies controller will now change to:

```haskell
-- Web/Controller/Companies.hs
-- ...
import qualified Network.Wai.Parse as Wai

    action CreateCompanyAction = do
        -- Upload file. If no file provided, we error and short-circuit.
        let file = fileOrNothing "uploadedFile" |> fromMaybe (error "no file given")

        let options :: StoreFileOptions = def
                { directory = "uploaded_files"
                , contentDisposition = contentDispositionAttachmentAndFileName
                }

        -- Store the file, and get the signed URL.
        storedFile <- storeFileWithOptions file options
        signedUrl <- createTemporaryDownloadUrl storedFile

        -- Create the UploadedFile record. Set the signed URL, path, content-type etc.
        uploadedFile <- newRecord @UploadedFile
                |> set #signedUrl signedUrl.url
                |> set #signedUrlExpiredAt signedUrl.expiredAt
                |> set #path storedFile.path
                |> set #fileName (cs file.fileName)
                |> set #contentType (cs $ Wai.fileContentType file)
                |> createRecord

        let company = newRecord @Company
        company
            |> buildCompany
            -- Reference the newly created UploadedFile record.
            |> set #uploadedFileId uploadedFile.id
            |> ifValid \case
                Left company -> render NewView { .. }
                Right company -> do
                    company <- company |> createRecord
                    setSuccessMessage "Company created"
                    redirectTo ShowCompanyAction { companyId = company.id }
```

Your HTML form should now have a file field as part of the HTML.

```haskell
renderForm :: Company -> Html
renderForm company = formFor company [hsx|
    {(textField #name)}

    <h2>File upload</h2>
    <input type="file" name="uploadedFile" />

    {submitButton}
|]
```

Our Show action can now change to the following code, that will get the `UploadedFile` out of the company,
and ensure we have an up-to-date signed URL.

```haskell
-- Web/Controller/Companies.hs
import IHP.FileStorage.ControllerFunctions
    action ShowCompanyAction { companyId } = do
        company <- fetch companyId

        -- Get the UploadedFile record.
        uploadedFile <- fetch company.uploadedFileId

        -- Refresh the signed URL, if needed.
        uploadedFile <- refreshTemporaryDownloadUrlFromFile uploadedFile

        render ShowView { .. }
```

The company show would now receive the following records:

```haskell
-- Web/View/Companies/Show.hs
data ShowView = ShowView { company :: Company, uploadedFile :: UploadedFile }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <a href={uploadedFile.signedUrl}>{uploadedFile.fileName}</a>
    |]
```

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

After that run `devenv up` and restart your development server.

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

You can use the [`refreshTemporaryDownloadUrl`](https://ihp.digitallyinduced.com/api-docs/IHP-FileStorage-ControllerFunctions.html#v:refreshTemporaryDownloadUrl) function to refresh the signed url. See the [example above](#required-uploads-with-a-file-record) for an example.

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

Let's see how we can implement that. At first, without any access control. We add an action to the `ImageStyleController` that accepts the original image path, the desired width, and height.

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

import Control.Exception (SomeException, try)
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import Data.Either (isRight)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.UUID as UUID (fromText)
import IHP.ControllerSupport
import qualified Network.Minio as Minio
import System.Directory (doesFileExist)
import Web.Controller.Prelude

instance Controller ImageStyleController where
    action RenderImageStyleAction {width, height, originalImagePath} = do
        let size = show width <> "x" <> show height
            (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath
            imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
            imageStylePath = imageStylePathDirectory <> "/" <> uuid
            originalImageDirectoryWithoutStoragePrefix = Text.replace storagePrefix "" originalImageDirectory
            imageStylePathDirectoryWithoutStoragePrefix = Text.replace storagePrefix "" imageStylePathDirectory
            imageStylePathWithoutStoragePrefix = Text.replace storagePrefix "" imageStylePath

        case storage of
            StaticDirStorage {} -> do
                fileExists <- doesFileExist (cs $ storagePrefix <> imageStylePath)
                if fileExists
                    then renderFile (cs $ storagePrefix <> imageStylePath) "application/jpg"
                    else do
                        let objectPath = cs $ storagePrefix <> originalImageDirectory <> "/" <> uuid
                        processAndStore uuid objectPath imageStylePathDirectory size (Nothing, Nothing)
            S3Storage {connectInfo, bucket} -> do
                let tempFileName = cs uuid
                    tempFilePath = "/tmp/" <> tempFileName
                    objectPath = originalImageDirectoryWithoutStoragePrefix <> "/" <> cs uuid

                remoteFileExists <-
                    Minio.runMinio connectInfo
                        $ Minio.statObject bucket imageStylePathWithoutStoragePrefix Minio.defaultGetObjectOptions

                case remoteFileExists of
                    -- File doesn't exist, so we need to process and store it.
                    Left _ -> do
                        -- Download original image into a temp file
                        downloadResult <-
                            Minio.runMinio connectInfo
                                $ do
                                    response <- Minio.getObject bucket objectPath Minio.defaultGetObjectOptions
                                    let objectInfo = Minio.gorObjectInfo response
                                    let metadata = Minio.oiMetadata objectInfo
                                    let maybeContentDisposition = HM.lookup "Content-Disposition" metadata
                                    let maybeContentType = HM.lookup "Content-Type" metadata
                                    -- Write the stream to the temp file
                                    C.connect (Minio.gorObjectStream response) (CB.sinkFileCautious tempFilePath)
                                    pure (maybeContentDisposition, maybeContentType)

                        case downloadResult of
                            Left err ->
                                error $ "Failed to download original image from S3: " <> show err
                            Right (maybeContentDisposition, maybeContentType) ->
                                -- Hand off the local file for resizing + re‐upload
                                processAndStore uuid tempFilePath imageStylePathDirectoryWithoutStoragePrefix size (maybeContentDisposition, maybeContentType)
                    Right signedUrl -> do
                        signedUrl <- createTemporaryDownloadUrlFromPath imageStylePathWithoutStoragePrefix
                        -- Redirect, to serve the file from S3.
                        redirectToUrl (cs signedUrl.url)

processAndStore :: (?context :: ControllerContext) => Text -> FilePath -> Text -> Text -> (Maybe Text, Maybe Text) -> IO ()
processAndStore uuid sourcePath imageStylePathDirectory size (maybeContentDisposition, maybeContentType) = do
    let options :: StoreFileOptions =
            def
                { directory = imageStylePathDirectory
                , preprocess =
                    applyImageMagick
                        "jpg"
                        ["-resize", cs size <> "^", "-gravity", "center", "-extent", cs size, "-quality", "85%", "-strip"]
                , fileName = UUID.fromText uuid
                , contentDisposition = \_ -> pure maybeContentDisposition
                }
    storedFile <- storeFileFromPath (cs sourcePath) options

    let contentType = cs $ fromMaybe "application/jpg" maybeContentType

    case storage of
        StaticDirStorage {} ->
            -- Render the local file.
            renderFile (cs $ storagePrefix <> storedFile.path) contentType
        S3Storage {connectInfo, bucket, baseUrl} -> do
            signedUrl <- createTemporaryDownloadUrlFromPath storedFile.path
            -- Redirect, to serve the file from S3.
            redirectToUrl (cs signedUrl.url)

-- | Extracts the directory and UUID from a path like "pictures/8ed22caa-11ea-4c45-a05e-91a51e72558d"
extractDirectoryAndUUID :: (?context :: context, ConfigProvider context) => Text -> (Text, Text)
extractDirectoryAndUUID inputText =
    case reverse parts of
        rawUuid : pathSegments ->
            -- Ensure we have only the UUID without any query parameters.
            let uuid = Text.take 36 rawUuid
             in (Text.intercalate "/" (reverse pathSegments), uuid)
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

The above implementation is not secure. Anyone can request any image style of any image. This may put a burden on the site. Imagine a malicious user requesting thousands of different dimensions of a 100MB image. The server would have to generate thousands of images. This would put a lot of load on the server. To prevent this, we need to add some access control. The way we would do it, is to add a signed token to the image style URL. The token would be generated by the server and would contain the original image path, the desired width and height. The server would then verify the token and only serve the image style if the token is valid. This way, the server would not have to generate the image style on the fly, but only if the token is valid. If the token is not valid, then the server would return an access denied.

Another concern we have is what if somehow the path will get have `../` in it, and the user will be able to traverse the file system. We need to prevent that as well.

We start by adding the `jwt` package to the `flake.nix` file.

```nix
haskellPackages = p: with p; [
    # Haskell dependencies go here
    p.ihp
    cabal-install
    base
    # ...
    jwt # <-- ADD THIS LINE
```

Locally we need to generate a private and public key pair. We execute from the root of the project the following command (don't add a passphrase):

```bash
ssh-keygen -t rsa -b 4096 -m PEM -f ./Config/jwtRS256.key
openssl rsa -in ./Config/jwtRS256.key -pubout -outform PEM -out ./Config/jwtRS256.key.pub
```

```haskell
-- Config/Config.hs
import Control.Exception (catch)
import qualified Data.ByteString as BS
import Web.JWT
import "cryptonite" Crypto.PubKey.RSA as RSA

data RsaKeys = RsaKeys { publicKey :: RSA.PublicKey, privateKey :: RSA.PrivateKey }

config :: ConfigBuilder
config = do
    -- ...

    -- Private and public keys to sign and verify image style URLs.
    privateKeyContent <- liftIO $ readRsaKeyFromFile "./Config/jwtRS256.key"
    publicKeyContent <- liftIO $ readRsaKeyFromFile "./Config/jwtRS256.key.pub"

    case (readRsaSecret privateKeyContent, readRsaPublicKey publicKeyContent) of
        (Just privateKey, Just publicKey) -> option $ RsaKeys publicKey privateKey
        _ -> error "Failed to read RSA keys, please execute from the root of your project: ssh-keygen -t rsa -b 4096 -m PEM -f ./Config/jwtRS256.key && openssl rsa -in ./Config/jwtRS256.key -pubout -outform PEM -out ./Config/jwtRS256.key.pub"


readRsaKeyFromFile :: FilePath -> IO BS.ByteString
readRsaKeyFromFile path = do
    catch (BS.readFile path) handleException
  where
    handleException :: IOError -> IO BS.ByteString
    handleException _ = return BS.empty

```

Add a few helper functions, a few for the Controllers.

```haskell
-- Application/Helper/Controller.hs

module Application.Helper.Controller where

-- ...
import Config
import Data.ByteString.Base64 as Base64
import "cryptonite" Crypto.PubKey.RSA as RSA
import "cryptonite" Crypto.PubKey.RSA.PKCS15 as RSA.PKCS15
import "cryptonite" Crypto.Hash.Algorithms as Hash.Algorithms


-- | The RSA public key, can be used to verify image style URLs that were signed.
rsaPublicKey :: (?context :: ControllerContext) => RSA.PublicKey
rsaPublicKey = (getAppConfig @Config.RsaKeys).publicKey

-- | The RSA private key, can be used to sign image style URLs.
rsaPrivateKey :: (?context :: ControllerContext) => RSA.PrivateKey
rsaPrivateKey = (getAppConfig @Config.RsaKeys).privateKey

rsaSignatureMatches :: (?context :: ControllerContext) =>  Text -> Text -> Bool
rsaSignatureMatches original signature = case Base64.decode $ cs signature of
    Left msg -> False
    Right decodedSignature -> RSA.PKCS15.verify (Just Hash.Algorithms.SHA256) rsaPublicKey (cs original) decodedSignature
```

And one for the View helper.

```haskell
-- Application/Helper/View.hs
module Application.Helper.View where

import IHP.ViewPrelude
import Application.Helper.Controller
import Data.ByteString.Base64 as Base64
import "cryptonite" Crypto.PubKey.RSA.PKCS15 as RSA
import "cryptonite" Crypto.Hash.Algorithms as Hash.Algorithms

-- Here you can add functions which are available in all your views

-- | Sign the image URL to prevent tampering.
signImageUrl :: (?context::ControllerContext) => Text -> Int -> Int -> Text
signImageUrl imageUrl width height= case RSA.sign Nothing (Just Hash.Algorithms.SHA256) rsaPrivateKey (cs $ imageUrl <> size) of
    Left msg -> error $ "Cannot sign image URL, private key is invalid:" <> show msg
    Right signature -> signature |> Base64.encode |> cs
    where
        size = show width <> "x" <> show height
```

Your Controller will now look like this:

```haskell
-- Web/Controller/ImageStyle.hs
module Web.Controller.ImageStyle where

import Web.Controller.Prelude
import IHP.ControllerSupport
import System.Directory (doesFileExist)
import qualified Data.Text as Text

-- Imports for the signed token.
import Config
import Data.ByteString.Base64 as Base64
import "cryptonite" Crypto.PubKey.RSA.PKCS15 as RSA
import "cryptonite" Crypto.Hash.Algorithms as Hash.Algorithms

instance Controller ImageStyleController where
    action RenderImageStyleAction { width, height, originalImagePath, signed } = do
        let size = show width <> "x" <> show height

        -- Verify the token, and deny or allow access based on the result.
        -- Also, deny access if there's `../` in the path, to prevent traversal attacks.
        accessDeniedUnless (rsaSignatureMatches (originalImagePath <> size) signed && not (Text.isInfixOf "../" originalImagePath))

        -- Get the original image directory and UUID from the path.
        let (originalImageDirectory, uuid) = extractDirectoryAndUUID originalImagePath

        let imageStylePathDirectory = originalImageDirectory <> "/imageStyles/" <> size
        let imageStylePath = imageStylePathDirectory <> "/" <> uuid

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
    signed = signImageUrl imageUrl 400 200
```

In order for the private and public keys to be available on your server, you should add the following to your `flake.nix`:

```nix
# ...
services.ihp = {
        # ...
    additionalEnvVars = {
        # The location of the RSA generated files.
        # Files are created below, see `systemd.services.app.preStart`.
        JWT_PRIVATE_KEY_PATH = "/root/jwtRS256.key";
        JWT_PUBLIC_KEY_PATH = "/root/jwtRS256.key.pub";
    }
}

# Create RSA keys for JWT authentication.
# See for example https://ihp.digitallyinduced.com/Guide/file-storage.html#image-style-implementation
systemd.services.app.preStart = ''
        if [ ! -f /root/jwtRS256.key ]; then
        # Generate the private key
        ${pkgs.openssl}/bin/openssl genpkey -algorithm RSA -out /root/jwtRS256.key -pkeyopt rsa_keygen_bits:4096
        # Extract the public key from the private key
        ${pkgs.openssl}/bin/openssl rsa -pubout -in /root/jwtRS256.key -out /root/jwtRS256.key.pub
        fi
    '';
```
