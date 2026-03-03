{ mkDerivation, aeson, base, bytestring, fetchgit, hasql, iproute
, lib, scientific, text, time, uuid
}:
mkDerivation {
  pname = "hasql-mapping";
  version = "0";
  src = fetchgit {
    url = "https://github.com/nikita-volkov/hasql-mapping";
    sha256 = "1ww54his5d3wfh3amdk9zk5w6v4pdgljlzifnqga3lwn1gasbsvr";
    rev = "307dfb5f25ba28d8408fac3aa160ca4ba702acc9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base bytestring hasql iproute scientific text time uuid
  ];
  homepage = "https://github.com/nikita-volkov/hasql-mapping";
  description = "SDK for defining modular mappings to databases on top of Hasql";
  license = lib.licenses.mit;
}
