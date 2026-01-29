{ mkDerivation, base, lib, typerep-map }:
mkDerivation {
  pname = "ihp-context";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [ base typerep-map ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Minimal typed context container for IHP";
  license = lib.licenses.mit;
}
