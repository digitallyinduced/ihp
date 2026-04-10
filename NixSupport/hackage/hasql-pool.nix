{ mkDerivation, async, base, bytestring, hasql, hspec
, hspec-discover, lib, postgresql-libpq, random, rerebase, stm
, testcontainers-postgresql, text, text-builder, time, tuple, uuid
}:
mkDerivation {
  pname = "hasql-pool";
  version = "1.4.2";
  sha256 = "ed83a674ae6ca875d5ed9f23e34f8a012b432d80fbcf625f5a345eb2bde73070";
  libraryHaskellDepends = [
    base bytestring hasql stm text time uuid
  ];
  testHaskellDepends = [
    async hasql hspec postgresql-libpq random rerebase
    testcontainers-postgresql text-builder tuple
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nikita-volkov/hasql-pool";
  description = "Pool of connections for Hasql";
  license = lib.licenses.mit;
}
