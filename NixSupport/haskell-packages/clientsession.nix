{ mkDerivation, base, base64-bytestring, bytestring, cereal
, containers, crypto-api, cryptonite, directory, entropy, hspec
, HUnit, lib, QuickCheck, setenv, skein, tagged, transformers
}:
mkDerivation {
  pname = "clientsession";
  version = "0.9.2.0";
  sha256 = "00z577s6z0h3pfd809xwqhm8gbb49a1pm6rramf9n0j7i9pxyqc3";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base64-bytestring bytestring cereal crypto-api cryptonite
    directory entropy setenv skein tagged
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring cereal containers hspec HUnit QuickCheck
    transformers
  ];
  homepage = "http://github.com/yesodweb/clientsession/tree/master";
  description = "Securely store session data in a client-side cookie";
  license = lib.licenses.mit;
  mainProgram = "clientsession-generate";
}