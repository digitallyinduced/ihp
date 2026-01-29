{ mkDerivation, base, bytestring, filepath, hspec, lib, megaparsec
, parser-combinators, string-conversions, text
}:
mkDerivation {
  pname = "ihp-postgres-parser";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring filepath megaparsec parser-combinators
    string-conversions text
  ];
  testHaskellDepends = [
    base bytestring filepath hspec megaparsec parser-combinators
    string-conversions text
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "PostgreSQL DDL parser and compiler";
  license = lib.licenses.mit;
}
