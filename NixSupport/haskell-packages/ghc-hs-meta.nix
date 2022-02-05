{ mkDerivation, base, bytestring, fetchzip, ghc, ghc-boot, hspec
, lib, template-haskell
}:
mkDerivation {
  pname = "ghc-hs-meta";
  version = "0.1.0.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/ghc-hs-meta/archive/refs/heads/master.zip";
    sha256 = "1lp46js6r9dab1p056rq5h6vl8srnqnjs4wkdvafgb0xlr2rwi9k";
  };
  libraryHaskellDepends = [
    base bytestring ghc ghc-boot template-haskell
  ];
  testHaskellDepends = [
    base bytestring ghc ghc-boot hspec template-haskell
  ];
  description = "Translate Haskell source to Template Haskell expression";
  license = lib.licenses.bsd3;
}
