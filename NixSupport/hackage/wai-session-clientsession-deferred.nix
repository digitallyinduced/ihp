{ mkDerivation, base, bytestring, cereal, clientsession, lib
, transformers, wai-session-maybe
}:
mkDerivation {
  pname = "wai-session-clientsession-deferred";
  version = "1.0.0";
  sha256 = "345c34a7e8b31fb255e3ec63c3a64ff8e7bf384f8ee0f73265a7e504c85e4717";
  libraryHaskellDepends = [
    base bytestring cereal clientsession transformers wai-session-maybe
  ];
  homepage = "https://github.com/digitallyinduced/wai-session-clientsession-deferred";
  description = "Session store based on clientsession with deferred decryption";
  license = "unknown";
}
