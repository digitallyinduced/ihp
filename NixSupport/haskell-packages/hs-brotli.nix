{ mkDerivation
, base
, bytestring
, fetchgit
, ghc-prim
, lib
, QuickCheck
, quickcheck-instances
, tasty
, tasty-hunit
, tasty-quickcheck
}:
let
  nixpkgs = import <nixpkgs> { };
in
mkDerivation {
  pname = "hs-brotli";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/iand675/hs-brotli";
    sha256 = "1i4qaqn79m4jcg16pdm222xy6x97r5fv8ikyykf1nl79hs0dajrh";
    rev = "d7bce54b265883fb30a14d39d00cbf1c1308b2b1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/brotli; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  librarySystemDepends = [ nixpkgs.brotli ];
  libraryToolDepends = [ nixpkgs.pkg-config ];
  libraryPkgconfigDepends = [ nixpkgs.brotli ];
  testHaskellDepends = [ base bytestring QuickCheck quickcheck-instances tasty tasty-hunit tasty-quickcheck ];
  homepage = "https://github.com/iand675/hs-brotli#readme";
  description = "Compression and decompression in the brotli format";
  license = lib.licenses.bsd3;
}
