{ mkDerivation, base, directory, filepath, hasql, hasql-transaction
, hspec, ihp-postgres-parser, lib, string-conversions, temporary-ospath, text, with-utf8
}:
mkDerivation {
  pname = "ihp-migrate";
  version = "1.6.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base directory filepath hasql hasql-transaction ihp-postgres-parser
    string-conversions text with-utf8
  ];
  executableHaskellDepends = [
    base directory filepath hasql hasql-transaction string-conversions
    text with-utf8
  ];
  testHaskellDepends = [
    base directory filepath hasql hspec string-conversions temporary-ospath text
    with-utf8
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Provides the IHP migrate binary";
  license = lib.licenses.mit;
  mainProgram = "migrate";
}
