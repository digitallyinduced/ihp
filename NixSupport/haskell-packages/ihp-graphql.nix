{ mkDerivation, aeson, attoparsec, base, hspec, ihp, lib
, postgresql-simple, text, unordered-containers, bytestring
, ihp-ide
}:
mkDerivation {
  pname = "ihp-graphql";
  version = "v1.3.0";
  src = ./../../ihp-graphql;
  libraryHaskellDepends = [
    aeson attoparsec base ihp postgresql-simple text
    unordered-containers ihp-ide
  ];
  testHaskellDepends = [
    aeson attoparsec base hspec ihp postgresql-simple text
    unordered-containers bytestring ihp-ide
  ];
  description = "GraphQL support for IHP";
  license = lib.licenses.mit;
}