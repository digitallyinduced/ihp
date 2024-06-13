{ mkDerivation, ihp, ihp-ide, neat-interpolation, lib, with-utf8_1_1_0_0, megaparsec }:
mkDerivation {
  pname = "ihp-datasync-typescript";
  version = "v1.3.0";
  src = ./../../ihp-datasync-typescript;
  libraryHaskellDepends = [
    ihp ihp-ide neat-interpolation
  ];
  testHaskellDepends = [
    ihp ihp-ide neat-interpolation megaparsec
  ];
  executableHaskellDepends = [ ihp with-utf8_1_1_0_0 ];
  description = "TypeScript code generation for IHP DataSync";
  license = lib.licenses.mit;
}