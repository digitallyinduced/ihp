{ mkDerivation, async, base, bytestring, bytestring-tree-builder
, contravariant, hasql, lib, mtl, rerebase, text, transformers
}:
mkDerivation {
  pname = "hasql-transaction";
  version = "1.2.2";
  sha256 = "bf902c7a983035e44b6936d6acab0c0ba7548106fb040ba8bb12b058b34eacda";
  libraryHaskellDepends = [
    base bytestring bytestring-tree-builder contravariant hasql mtl
    text transformers
  ];
  testHaskellDepends = [ async hasql rerebase ];
  homepage = "https://github.com/nikita-volkov/hasql-transaction";
  description = "Composable abstraction over retryable transactions for Hasql";
  license = lib.licenses.mit;
}
