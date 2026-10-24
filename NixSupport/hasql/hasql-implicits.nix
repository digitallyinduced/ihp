{ mkDerivation, aeson, base, bytestring, containers, hasql, iproute
, lib, scientific, source, text, time, uuid, vector
}:
mkDerivation {
  pname = "hasql-implicits";
  version = "0.2.0.3";
  src = source;
  libraryHaskellDepends = [
    aeson base bytestring containers hasql iproute scientific text time
    uuid vector
  ];
  homepage = "https://github.com/nikita-volkov/hasql-implicits";
  description = "Implicit definitions for Hasql, such as default codecs for standard types";
  license = lib.licenses.mit;
}
