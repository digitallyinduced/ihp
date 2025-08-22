{ mkDerivation, aeson, attoparsec, base, hspec, ihp, lib
, postgresql-simple, text, unordered-containers, bytestring
, ihp-ide, countable-inflections
}:
mkDerivation {
  pname = "ihp-graphql";
  version = "v1.3.0";
  src = ./../../ihp-graphql;
  libraryHaskellDepends = [
    aeson attoparsec base ihp postgresql-simple text
    unordered-containers ihp-ide countable-inflections
  ];
  testHaskellDepends = [
    aeson attoparsec base hspec ihp postgresql-simple text
    unordered-containers bytestring ihp-ide countable-inflections
  ];
  description = "GraphQL support for IHP";
  license = lib.licenses.mit;
}