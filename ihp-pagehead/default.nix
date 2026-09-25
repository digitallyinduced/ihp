{ mkDerivation, base, blaze-html, ihp-hsx, lib, text, vault, wai }:
mkDerivation {
  pname = "ihp-pagehead";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base blaze-html ihp-hsx text vault wai ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Page title and meta tags for IHP";
  license = lib.licenses.mit;
}
