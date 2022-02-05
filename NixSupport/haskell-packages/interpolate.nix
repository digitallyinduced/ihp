{ mkDerivation, base, base-compat, bytestring, fetchzip
, ghc-hs-meta, hspec, lib, QuickCheck, quickcheck-instances
, template-haskell, text
}:
mkDerivation {
  pname = "interpolate";
  version = "0.2.1";
  src = fetchzip {
    url = "https://github.com/zacwood9/interpolate/archive/refs/heads/master.zip";
    sha256 = "0ry556hicyxf35ffhrl3gb9z5hqjgiakykw4l8rxyri1fh6hwqsq";
  };
  libraryHaskellDepends = [ base ghc-hs-meta template-haskell ];
  testHaskellDepends = [
    base base-compat bytestring ghc-hs-meta hspec QuickCheck
    quickcheck-instances template-haskell text
  ];
  homepage = "https://github.com/sol/interpolate#readme";
  description = "String interpolation done right";
  license = lib.licenses.mit;
}
