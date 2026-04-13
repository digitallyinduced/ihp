{ mkDerivation, base, bytestring, charset, containers, hashable
, hspec, lib, megaparsec, QuickCheck, quickcheck-classes, text
, text-builder
}:
mkDerivation {
  pname = "postgresql-connection-string";
  version = "0.1.0.6";
  sha256 = "2deb48d7f483b28d39f8d7d24d2ad70e69db5042dbd0351528e4f976acdab110";
  libraryHaskellDepends = [
    base bytestring charset containers hashable megaparsec QuickCheck
    text text-builder
  ];
  testHaskellDepends = [
    base containers hspec QuickCheck quickcheck-classes text
  ];
  homepage = "https://github.com/nikita-volkov/postgresql-connection-string";
  description = "PostgreSQL connection string type, parser and builder";
  license = lib.licenses.mit;
}
