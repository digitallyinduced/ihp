{ mkDerivation, base, bytestring, case-insensitive, clock
, containers, criterion, fetchzip, hashable, headed-megaparsec
, hspec, hspec-discover, lib, megaparsec, parser-combinators
, QuickCheck, rerebase, text, text-builder, unordered-containers
}:
mkDerivation {
  pname = "postgresql-syntax";
  version = "0.5.0.3";
  src = fetchzip {
    url = "https://hackage.haskell.org/package/postgresql-syntax-0.5.0.3/postgresql-syntax-0.5.0.3.tar.gz";
    sha256 = "1ivnig1rc2gyihhx2g4ny9y8xnmvjrcs58bxkly1rr3pszcfqkr8";
  };
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
  license = lib.licenses.mit;
}
