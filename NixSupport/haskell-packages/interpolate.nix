{ mkDerivation, base, base-compat, bytestring, fetchzip, ghc-meta
, hspec, lib, QuickCheck, quickcheck-instances, template-haskell
, text
}:
mkDerivation {
  pname = "interpolate";
  version = "0.2.1";
  src = fetchzip {
    url = "https://github.com/zacwood9/interpolate/archive/refs/heads/master.zip";
    sha256 = "0mvk89l94jacb2z6ipxsyrd4w2skbikj1xm3478r1s0wqajk0f97";
  };
  libraryHaskellDepends = [ base ghc-meta template-haskell ];
  testHaskellDepends = [
    base base-compat bytestring ghc-meta hspec QuickCheck
    quickcheck-instances template-haskell text
  ];
  homepage = "https://github.com/sol/interpolate#readme";
  description = "String interpolation done right";
  license = lib.licenses.mit;
}
