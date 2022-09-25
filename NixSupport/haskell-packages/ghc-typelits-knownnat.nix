{ mkDerivation, base, ghc, ghc-prim, ghc-tcplugins-extra
, ghc-typelits-natnormalise, lib, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, transformers
, fetchFromGitHub
}:
mkDerivation {
  pname = "ghc-typelits-knownnat";
  version = "0.7.6";
  sha256 = "10m4y0hf25w2i40464pz85lqs5dr8cznl191icnibc5fjynyzd9v";
  libraryHaskellDepends = [
    base ghc ghc-prim ghc-tcplugins-extra ghc-typelits-natnormalise
    template-haskell transformers
  ];
  testHaskellDepends = [
    base ghc-typelits-natnormalise tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "http://clash-lang.org/";
  description = "Derive KnownNat constraints from other KnownNat constraints";
  license = lib.licenses.bsd2;
  src = fetchFromGitHub {
    owner = "clash-lang";
    repo = "ghc-typelits-knownnat";
    rev = "941-support";
    sha256 = "sha256-9ITC1f0wGvOH1VrFXegwSUugjR777FknFcfOsNxXhMY";
  };
}