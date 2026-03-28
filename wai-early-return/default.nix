{ mkDerivation, base, hspec, http-types, lib, wai, wai-extra }:
mkDerivation {
  pname = "wai-early-return";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base wai ];
  testHaskellDepends = [ base hspec http-types wai wai-extra ];
  description = "WAI middleware for early return from request handlers";
  license = lib.licenses.mit;
}
