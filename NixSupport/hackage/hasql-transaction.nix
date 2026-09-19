{ mkDerivation, async, base, bytestring, bytestring-tree-builder
, contravariant, hasql, hspec, hspec-discover, lib, mtl, pqi
, pqi-ffi, pqi-native, rerebase, testcontainers-postgresql, text
, transformers
}:
mkDerivation {
  pname = "hasql-transaction";
  version = "1.2.3.1";
  sha256 = "873c525275e729e5e926f124d0576005ded60e5d79f4586b60a75ebd78b5707d";
  libraryHaskellDepends = [
    base bytestring bytestring-tree-builder contravariant hasql mtl
    text transformers
  ];
  testHaskellDepends = [
    async hasql hspec pqi pqi-ffi pqi-native rerebase
    testcontainers-postgresql
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nikita-volkov/hasql-transaction";
  description = "Composable abstraction over retryable transactions for Hasql";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
