{ mkDerivation, async, attoparsec, base, hspec, lib
, postgresql-simple, postgresql-types, postgresql-types-algebra
, QuickCheck, quickcheck-instances, stm, tagged
, testcontainers-postgresql, text, text-builder
}:
mkDerivation {
  pname = "postgresql-simple-postgresql-types";
  version = "0.1.1";
  sha256 = "25ed8f8ec444be55597bf4ed09bae149f854a607aeaa90c2b7cb191a74db481a";
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
  license = lib.licenses.mit;
}
