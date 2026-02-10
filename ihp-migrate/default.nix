{ mkDerivation, base, directory, filepath, hasql, hasql-transaction
, hspec, lib, string-conversions, temporary, text, with-utf8
}:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath hasql hasql-transaction string-conversions
    text with-utf8
  ];
  executableHaskellDepends = [
    base directory filepath hasql hasql-transaction string-conversions
    text with-utf8
  ];
  testHaskellDepends = [
    base directory filepath hspec temporary with-utf8
  ];
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}
