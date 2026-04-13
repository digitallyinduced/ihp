{ mkDerivation, base, bytestring, containers, hasql
, hasql-implicits, hspec, hspec-discover, lib, rerebase
, testcontainers-postgresql, text, text-builder
}:
mkDerivation {
  pname = "hasql-dynamic-statements";
  version = "0.5.1";
  sha256 = "979644ad93b1f4f46e5c62a24ddf3debbe2acd61fa8e8b239cfdeef0d92d88cd";
  libraryHaskellDepends = [
    base bytestring containers hasql hasql-implicits text text-builder
  ];
  testHaskellDepends = [
    hasql hspec rerebase testcontainers-postgresql
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nikita-volkov/hasql-dynamic-statements";
  description = "Hasql extension for dynamic construction of statements";
  license = lib.licenses.mit;
}
