{ mkDerivation, async, attoparsec, base, fetchgit, hspec, lib
, postgresql-simple, postgresql-types, postgresql-types-algebra
, QuickCheck, quickcheck-instances, stm, tagged
, testcontainers-postgresql, text, text-builder
}:
mkDerivation {
  pname = "postgresql-simple-postgresql-types";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/nikita-volkov/postgresql-simple-postgresql-types";
    sha256 = "0hrlfbqnhf8jcc36hnq62mp6jnh9w1m2n1i4b9qvd3mdzy4w7knx";
    rev = "c786a1fbb5435544d3fb7199dac02fb73b25749f";
    fetchSubmodules = true;
  };
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
