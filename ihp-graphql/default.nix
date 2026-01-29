{ mkDerivation, aeson, attoparsec, base, bytestring
, countable-inflections, hspec, ihp, ihp-postgres-parser, lib
, megaparsec, postgresql-simple, text, unordered-containers
}:
mkDerivation {
  pname = "ihp-graphql";
  version = "1.3.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base countable-inflections ihp ihp-postgres-parser
    postgresql-simple text unordered-containers
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring countable-inflections hspec ihp
    ihp-postgres-parser megaparsec postgresql-simple text
    unordered-containers
  ];
  description = "GraphQL support for IHP";
  license = lib.licenses.mit;
}
