{ mkDerivation, base, containers, ghc, ghc-tcplugins-extra
, integer-gmp, lib, tasty, tasty-hunit, template-haskell
, transformers, ghc-bignum, fetchFromGitHub
}:
mkDerivation {
  pname = "ghc-typelits-natnormalise";
  version = "0.7.7";
  sha256 = "09d70iw58m5g6yi8k2b52f1g0pfdqm5fzhs8rd7fgrgmi70np9bx";
  libraryHaskellDepends = [
    base containers ghc ghc-tcplugins-extra integer-gmp transformers ghc-bignum
  ];
  testHaskellDepends = [ base tasty tasty-hunit template-haskell ];
  homepage = "http://www.clash-lang.org/";
  description = "GHC typechecker plugin for types of kind GHC.TypeLits.Nat";
  license = lib.licenses.bsd2;
  src = fetchFromGitHub {
    owner = "clash-lang";
    repo = "ghc-typelits-natnormalise";
    rev = "64f3fe6bbc1a3c8d729ede7c9f14b158e2a12641";
    sha256 = "sha256-cHDm8Bd5VhN3hhCBQYFfaGl3uczC46CTbM1mIKZP9bY";
  };
}