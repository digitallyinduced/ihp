{ mkDerivation
, classy-prelude
, string-conversions
, blaze-html
, blaze-markup
, text
, bytestring
, lib
, basic-prelude
, megaparsec
, template-haskell
, haskell-src-meta
, containers
}:
mkDerivation {
  pname = "ihp-hsx";
  version = "v0.18.0";
  src = ./../../ihp-hsx;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    classy-prelude
    string-conversions
    blaze-html
    blaze-markup
    text
    bytestring
    basic-prelude
    megaparsec
    template-haskell
    haskell-src-meta
    containers
  ];
  license = lib.licenses.mit;
  enableLibraryForGhci = true;
  homepage = "https://ihp.digitallyinduced.com";
  configureFlags = ["--enable-optimization=2"];
}
