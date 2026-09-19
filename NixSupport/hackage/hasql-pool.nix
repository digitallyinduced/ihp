{ mkDerivation, async, base, bytestring, hasql, hspec
, hspec-discover, lib, pqi, pqi-ffi, pqi-native, random, rerebase
, stm, testcontainers-postgresql, text, text-builder, time, tuple
, uuid
}:
mkDerivation {
  pname = "hasql-pool";
  version = "1.5.0.1";
  sha256 = "cd0b73cc0c5bc0555098605f6e29235d675950a77517065762dd31c831f15ca3";
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
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
