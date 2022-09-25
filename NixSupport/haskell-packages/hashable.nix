{ mkDerivation, base, base-orphans, bytestring, containers, deepseq
, ghc-bignum, ghc-bignum-orphans, ghc-prim, HUnit, lib, QuickCheck
, random, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, unix
}:
mkDerivation {
  pname = "hashable";
  version = "1.4.1.0";
  sha256 = "11sycr73821amdz8g0k8c97igi4z7f9xdvgaxlkxhsp6h310bcz1";
  libraryHaskellDepends = [
    base base-orphans bytestring containers deepseq ghc-bignum
    ghc-bignum-orphans ghc-prim text
  ];
  testHaskellDepends = [
    base bytestring ghc-prim HUnit QuickCheck random test-framework
    test-framework-hunit test-framework-quickcheck2 text unix
  ];
  homepage = "http://github.com/haskell-unordered-containers/hashable";
  description = "A class for types that can be converted to a hash value";
  license = lib.licenses.bsd3;
}