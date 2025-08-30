{ mkDerivation, ihp, lib, with-utf8 }:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.4.0";
  src = ./../../ihp-migrate;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ ihp with-utf8 ];
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}