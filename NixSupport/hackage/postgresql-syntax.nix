{ mkDerivation, base, bytestring, case-insensitive, clock
, containers, criterion, hashable, headed-megaparsec, hspec
, hspec-discover, lib, megaparsec, parser-combinators, QuickCheck
, rerebase, text, text-builder, unordered-containers
}:
mkDerivation {
  pname = "postgresql-syntax";
  version = "0.5.0.3";
  sha256 = "6d0d87d72c5ea1cb7ecc21b57cf30d4cc44e31ad65c09ecb6c838cad989cda35";
  libraryHaskellDepends = [
    base bytestring case-insensitive hashable headed-megaparsec
    megaparsec parser-combinators QuickCheck text text-builder
    unordered-containers
  ];
  testHaskellDepends = [ hspec megaparsec QuickCheck rerebase ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    clock containers criterion headed-megaparsec megaparsec QuickCheck
    rerebase
  ];
  doHaddock = false;
  homepage = "https://github.com/nikita-volkov/postgresql-syntax";
  description = "PostgreSQL AST parsing and rendering";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
