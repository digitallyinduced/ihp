{ mkDerivation, async, base, bytestring, bytestring-tree-builder
, contravariant, hasql, hspec, hspec-discover, lib, mtl, pqi
, pqi-ffi, pqi-native, rerebase, source, testcontainers-postgresql
, text, transformers
}:
mkDerivation {
  pname = "hasql-transaction";
  version = "1.2.3.1";
  src = source;
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
  license = lib.licenses.mit;
}
