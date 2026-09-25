{ mkDerivation, base, bytestring, filepath, lib, process, temporary-ospath, text
, wai-extra
}:
mkDerivation {
  pname = "ihp-imagemagick";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring filepath process temporary-ospath text wai-extra
  ];
  description = "ImageMagick preprocessing for IHP file uploads";
  license = lib.licenses.mit;
}
