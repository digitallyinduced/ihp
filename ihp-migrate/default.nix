{ mkDerivation, base, directory, filepath, hasql
, hasql-dynamic-statements, hasql-pool, hasql-transaction, hspec
, ihp, ihp-log, lib, temporary, text, with-utf8
}:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath hasql hasql-dynamic-statements hasql-pool
    hasql-transaction ihp ihp-log text with-utf8
  ];
  executableHaskellDepends = [
    base directory filepath hasql hasql-dynamic-statements hasql-pool
    hasql-transaction ihp ihp-log text with-utf8
  ];
  testHaskellDepends = [
    base directory filepath hspec ihp temporary with-utf8
  ];
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}
