{ mkDerivation, base, bytestring, lib, process, temporary, text
, wai-extra
}:
mkDerivation {
  pname = "ihp-imagemagick";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring process temporary text wai-extra
  ];
  description = "ImageMagick preprocessing for IHP file uploads";
  license = lib.licenses.mit;
}
