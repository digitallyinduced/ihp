{ mkDerivation, async, attoparsec, base, hspec, lib
, postgresql-simple, postgresql-types, postgresql-types-algebra
, QuickCheck, quickcheck-instances, stm, tagged
, testcontainers-postgresql, text, text-builder
}:
mkDerivation {
  pname = "postgresql-simple-postgresql-types";
  version = "0.1.3.0";
  sha256 = "8d0eaa2f5e4f2d25eb68c778e872d840805c43a39ce0aa7f125e42de041c1ac2";
  libraryHaskellDepends = [
    attoparsec base postgresql-simple postgresql-types
    postgresql-types-algebra tagged text text-builder
  ];
  testHaskellDepends = [
    async base hspec postgresql-simple postgresql-types
    postgresql-types-algebra QuickCheck quickcheck-instances stm tagged
    testcontainers-postgresql text
  ];
  homepage = "https://github.com/nikita-volkov/postgresql-simple-postgresql-types";
  description = "Integration of \"postgresql-simple\" with \"postgresql-types\"";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
