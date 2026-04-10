{ mkDerivation, base, bytestring, criterion, hspec, hspec-discover
, isomorphism-class, lib, numeric-limits, QuickCheck, rerebase
, scientific, text
}:
mkDerivation {
  pname = "ptr-poker";
  version = "0.1.3";
  sha256 = "0b41e14e6b6a195da6b2907cafe46905db3f9af13859e4753696ce682bc1ef29";
  libraryHaskellDepends = [ base bytestring scientific text ];
  testHaskellDepends = [
    hspec isomorphism-class numeric-limits QuickCheck rerebase
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ criterion rerebase ];
  homepage = "https://github.com/nikita-volkov/ptr-poker";
  description = "Pointer poking action construction and composition toolkit";
  license = lib.licenses.mit;
}
