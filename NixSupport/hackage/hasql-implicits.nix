{ mkDerivation, aeson, base, bytestring, containers, hasql, iproute
, lib, scientific, text, time, uuid, vector
}:
mkDerivation {
  pname = "hasql-implicits";
  version = "0.2.0.2";
  sha256 = "266332a1881860ffd002a44d2903359c66bbcee3fe01ebfb1bfd30198b9de63e";
  libraryHaskellDepends = [
    aeson base bytestring containers hasql iproute scientific text time
    uuid vector
  ];
  homepage = "https://github.com/nikita-volkov/hasql-implicits";
  description = "Implicit definitions for Hasql, such as default codecs for standard types";
  license = lib.licenses.mit;
}
