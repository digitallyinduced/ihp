{ mkDerivation, ihp, lib, with-utf8_1_1_0_0 }:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.3.0";
  src = ./../../ihp-migrate;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ ihp with-utf8_1_1_0_0 ];
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}