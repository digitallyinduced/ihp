{ mkDerivation, aeson, attoparsec, base, basic-prelude, bytestring
, hspec, hspec-discover, ip, lib, postgresql-simple, text, time
, time-compat
}:
mkDerivation {
  pname = "ihp-postgresql-simple-extra";
  version = "v1.1.0";
  src = ./../../ihp-postgresql-simple-extra;
  libraryHaskellDepends = [
    aeson attoparsec base basic-prelude bytestring ip postgresql-simple
    text time time-compat
  ];
  testHaskellDepends = [
    aeson attoparsec base basic-prelude bytestring hspec hspec-discover
    ip postgresql-simple text time time-compat
  ];
  testToolDepends = [ hspec-discover ];
  description = "Extra data types for postgresql-simple";
  license = lib.licenses.mit;
}