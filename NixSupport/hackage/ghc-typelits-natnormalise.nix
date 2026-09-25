{ mkDerivation, base, containers, ghc, ghc-bignum, ghc-prim
, ghc-tcplugin-api, interpolate, lib, process, tasty, tasty-hunit
, temporary, transformers
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.9.6";
  sha256 = "626bb0dc22714e959ea759101a36cf1c84d753c09ecc1fd691528c012e09c5d3";
  libraryHaskellDepends = [
    base containers ghc ghc-bignum ghc-tcplugin-api transformers
  ];
  testHaskellDepends = [
    base ghc-prim interpolate process tasty tasty-hunit temporary
  ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = lib.meta.getLicenseFromSpdxId "BSD-2-Clause";
}
