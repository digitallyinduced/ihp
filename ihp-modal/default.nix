{ mkDerivation, base, blaze-html, ihp-hsx, lib, text, vault, wai }:
mkDerivation {
  pname = "ihp-modal";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base blaze-html ihp-hsx text vault wai ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Modal dialog support for IHP applications";
  license = lib.licenses.mit;
}
