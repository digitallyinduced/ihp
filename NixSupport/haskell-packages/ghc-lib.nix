{ mkDerivation, alex, array, base, binary, bytestring, containers
, deepseq, directory, filepath, ghc-lib-parser, ghc-prim, happy
, hpc, lib, pretty, process, time, transformers, unix
, fetchFromGitHub
}:
mkDerivation {
  pname = "ghc-lib";
  version = "8.10.3.20201220";
  sha256 = "abd24d5e79ade3e043cc6eff1246c4f775bd533be03237bbdc88d493920cae11";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    ghc-lib-parser ghc-prim hpc pretty process time transformers unix
  ];
  libraryToolDepends = [ alex happy ];
  homepage = "https://github.com/digital-asset/ghc-lib";
  description = "The GHC API, decoupled from GHC versions";
  license = lib.licenses.bsd3;
  src = fetchFromGitHub {
    owner = "digital-asset";
    repo = "ghc-lib";
    rev = "e7dc9aa8aa444a7b79fe540c2edbed4d1a8fcf5b";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i8";
  };
}