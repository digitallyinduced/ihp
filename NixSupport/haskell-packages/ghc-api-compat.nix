{ mkDerivation, ghc, hashable, lib, fetchFromGitLab }:
mkDerivation {
  pname = "ghc-api-compat";
  version = "9.2";
  sha256 = "7a70211154601ec88cb1acc4e4c69f023a4258bbbb23e3e13bdf25977159b6df";
  libraryHaskellDepends = [ ghc hashable ];
  doHaddock = false;
  description = "GHC-API compatibility helpers";
  license = lib.licenses.bsd3;
  
  src = fetchFromGitLab {
    owner = "haskell";
    repo = "ghc-api-compat";
    rev = "d221dede2caa81b34f9bfb79f28c669546f0c2ba";
    sha256 = "sha256-6izwX1RslgkKISzwuG9RMxxvgpryb0ria8PjPzaBrBg";
  };
}