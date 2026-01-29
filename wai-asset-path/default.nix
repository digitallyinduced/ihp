{ mkDerivation, base, lib, text, vault, wai }:
mkDerivation {
  pname = "wai-asset-path";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text vault wai ];
  description = "assetPath function for WAI";
  license = lib.licenses.mit;
}
