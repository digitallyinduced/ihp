{ mkDerivation, aeson, base, bytestring, hasql, hasql-transaction
, iproute, lib, scientific, text, time, uuid
}:
mkDerivation {
  pname = "hasql-mapping";
  version = "0.1.1.0";
  sha256 = "68183a39afcb23034ffedfa9e9c661eb8787b1e5442b430e66908c882cbddf18";
  libraryHaskellDepends = [
    aeson base bytestring hasql hasql-transaction iproute scientific
    text time uuid
  ];
  homepage = "https://github.com/nikita-volkov/hasql-mapping";
  description = "SDK for defining modular mappings to databases on top of Hasql";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
