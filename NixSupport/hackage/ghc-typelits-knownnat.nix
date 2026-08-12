{ mkDerivation, base, ghc, ghc-bignum, ghc-tcplugin-api
, ghc-typelits-natnormalise, lib, QuickCheck, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.8.4";
  sha256 = "116c5b920ca7f477ffa9f4624fcf729bb4e09cbbe715d6ef1e0acca5e29023ad";
  libraryHaskellDepends = [
    base ghc ghc-bignum ghc-tcplugin-api ghc-typelits-natnormalise
    template-haskell transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-natnormalise QuickCheck tasty tasty-hunit
    tasty-quickcheck
  ];
  homepage = "http://clash-lang.org/";
  description = "Derive KnownNat constraints from other KnownNat constraints";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
