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
, unordered-containers
, hspec
}:
mkDerivation {
  pname = "ihp-hsx";
  version = "v1.3.0";
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
    containers
    unordered-containers
  ];
  testHaskellDepends = [ hspec ];
  license = lib.licenses.mit;
  enableLibraryForGhci = true;
  homepage = "https://ihp.digitallyinduced.com";
  configureFlags = ["--enable-optimization=2"];
  jailbreak = true;
}
