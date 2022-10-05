{ mkDerivation
, base
, bytestring
, fetchgit
, ghc-prim
, lib
}:
let
  nixpkgs = import <nixpkgs> { };
in
mkDerivation {
  pname = "hs-brotli";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/s0kil/hs-brotli";
    sha256 = "0xwp0zj28njl6f8x47rwbsngkbg6r9a05bhrdxhkls2gnbvx8dac";
    rev = "9f27b1f443d21d177100cd7d0906ad68020d9331";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/brotli; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  librarySystemDepends = [ nixpkgs.brotli ];
  libraryToolDepends = [ nixpkgs.pkg-config ];
  libraryPkgconfigDepends = [ nixpkgs.brotli ];
  doHaddock = false;
  doCheck = false;
  jailbreak = true;
  homepage = "https://github.com/s0kil/hs-brotli#readme";
  description = "Compression and decompression in the brotli format";
  license = lib.licenses.bsd3;
}
