{ mkDerivation, aeson, base, bytestring, hasql, iproute, lib
, scientific, text, time, uuid
}:
mkDerivation {
  pname = "hasql-mapping";
  version = "0.1";
  sha256 = "12d50e628b7eba317a189166682bf93bf6c7751860662fab4e4731527ec9f7ca";
  libraryHaskellDepends = [
    aeson base bytestring hasql iproute scientific text time uuid
  ];
  homepage = "https://github.com/nikita-volkov/hasql-mapping";
  description = "SDK for defining modular mappings to databases on top of Hasql";
  license = lib.licenses.mit;
}
