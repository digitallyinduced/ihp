{ mkDerivation, base, directory, filepath, hspec, ihp, ihp-log, lib
, temporary, text, with-utf8
}:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath ihp ihp-log text with-utf8
  ];
  executableHaskellDepends = [
    base directory filepath ihp ihp-log text with-utf8
  ];
  testHaskellDepends = [
    base directory filepath hspec ihp temporary with-utf8
  ];
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}
