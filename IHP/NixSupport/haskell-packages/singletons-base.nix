{ mkDerivation, base, bytestring, Cabal, deepseq, directory
, filepath, lib, pretty, process, singletons, singletons-th, tasty
, tasty-golden, template-haskell, text, th-desugar, turtle
}:
mkDerivation {
  pname = "singletons-base";
  version = "3.1";
  sha256 = "sha256-bZ4ZBDGSL1PmfnVPSL6/PiYMsgG9pDx0tAJH7lZbTq8";
  setupHaskellDepends = [ base Cabal directory filepath ];
  libraryHaskellDepends = [
    base pretty singletons singletons-th template-haskell text
    th-desugar
  ];
  testHaskellDepends = [
    base bytestring deepseq filepath process tasty tasty-golden text
    turtle
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A promoted and singled version of the base library";
  license = lib.licenses.bsd3;
}