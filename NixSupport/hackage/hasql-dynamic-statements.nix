{ mkDerivation, base, bytestring, containers, hasql
, hasql-implicits, hspec, hspec-discover, lib, pqi-native, rerebase
, testcontainers-postgresql, text, text-builder
}:
mkDerivation {
  pname = "hasql-dynamic-statements";
  version = "0.5.1.1";
  sha256 = "5a3b35e9de4eae55802eecf040fb7ab6f9c3100c078f792e15a017f29b1b2d9f";
  libraryHaskellDepends = [
    base bytestring containers hasql hasql-implicits text text-builder
  ];
  testHaskellDepends = [
    hasql hspec pqi-native rerebase testcontainers-postgresql
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/nikita-volkov/hasql-dynamic-statements";
  description = "Hasql extension for dynamic construction of statements";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
