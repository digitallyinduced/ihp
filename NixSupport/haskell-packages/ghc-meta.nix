{ mkDerivation, base, bytestring, fetchzip, ghc, ghc-boot, hspec
, lib, template-haskell
}:
mkDerivation {
  pname = "ghc-meta";
  version = "0.1.0.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/ghc-meta/archive/refs/heads/master.zip";
    sha256 = "1sglwqaw38v4zydvfczhkr99zxvnrrqmdiwl8676hp9bm5gcb07i";
  };
  libraryHaskellDepends = [
    base bytestring ghc ghc-boot template-haskell
  ];
  testHaskellDepends = [
    base bytestring ghc ghc-boot hspec template-haskell
  ];
  description = "Haskell source to Template Haskell expression";
  license = lib.licenses.bsd3;
}
