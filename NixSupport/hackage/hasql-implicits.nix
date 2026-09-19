{ mkDerivation, aeson, base, bytestring, containers, hasql, iproute
, lib, scientific, text, time, uuid, vector
}:
mkDerivation {
  pname = "hasql-implicits";
  version = "0.2.0.3";
  sha256 = "a7152fa44aab7b7797c260c3c1134a7b85f45514c0baaf538e1f3e104548a02f";
  libraryHaskellDepends = [
    aeson base bytestring containers hasql iproute scientific text time
    uuid vector
  ];
  homepage = "https://github.com/nikita-volkov/hasql-implicits";
  description = "Implicit definitions for Hasql, such as default codecs for standard types";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
