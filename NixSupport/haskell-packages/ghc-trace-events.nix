{ mkDerivation, base, bytestring, criterion, lib, text }:
mkDerivation {
  pname = "ghc-trace-events";
  version = "0.1.2.3";
  sha256 = "04e96bf39a867aacc2405eca05668f641dc74bba6bca3591f2e9a329b5df5d47";
  libraryHaskellDepends = [ base bytestring text ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/maoe/ghc-trace-events";
  description = "Faster traceEvent and traceMarker, and binary object logging for eventlog";
  license = lib.licenses.bsd3;
  jailbreak = true;
}