{ mkDerivation, async, base, bytestring, hasql, hspec
, hspec-discover, lib, pqi, pqi-ffi, pqi-native, random, rerebase
, source, stm, testcontainers-postgresql, text, text-builder, time
, tuple, uuid
}:
mkDerivation {
  pname = "hasql-pool";
  version = "1.5.0.1";
  src = source;
  libraryHaskellDepends = [
    base bytestring hasql pqi stm text time uuid
  ];
  testHaskellDepends = [
    async hasql hspec pqi pqi-ffi pqi-native random rerebase
    testcontainers-postgresql text-builder tuple
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nikita-volkov/hasql-pool";
  description = "Pool of connections for Hasql";
  license = lib.licenses.mit;
}
