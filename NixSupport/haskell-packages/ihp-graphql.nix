{ mkDerivation, aeson, attoparsec, base, hspec, ihp, lib
, postgresql-simple, text, unordered-containers, bytestring
}:
mkDerivation {
  pname = "ihp-graphql";
  version = "v1.2.0";
  src = ./../../ihp-graphql;
  libraryHaskellDepends = [
    aeson attoparsec base ihp postgresql-simple text
    unordered-containers
  ];
  testHaskellDepends = [
    aeson attoparsec base hspec ihp postgresql-simple text
    unordered-containers bytestring
  ];
  description = "GraphQL support for IHP";
  license = lib.licenses.mit;
}