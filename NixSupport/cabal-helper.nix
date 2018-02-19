{ mkDerivation, base, bytestring, Cabal, cabal-install, containers
, directory, exceptions, fetchgit, filepath, ghc, ghc-paths
, ghc-prim, mtl, process, semigroupoids, stdenv, template-haskell
, temporary, transformers, unix, unix-compat, utf8-string
}:
mkDerivation {
  pname = "cabal-helper";
  version = "0.8.0.1";
  src = fetchgit {
    url = "https://github.com/DanielG/cabal-helper/";
    sha256 = "1dk55893yxh0mhjwsvxx1dsbdil51v99130nfrlj0qgc31jqbysi";
    rev = "09e2236679739547984e2e29db23e160a3a80890";
  };
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal directory filepath ];
  libraryHaskellDepends = [
    base Cabal directory filepath ghc-prim mtl process semigroupoids
    transformers unix unix-compat
  ];
  executableHaskellDepends = [
    base bytestring Cabal containers directory exceptions filepath
    ghc-prim mtl process template-haskell temporary transformers unix
    unix-compat utf8-string
  ];
  executableToolDepends = [ cabal-install ];
  testHaskellDepends = [
    base bytestring Cabal directory exceptions filepath ghc ghc-paths
    ghc-prim mtl process template-haskell temporary transformers unix
    unix-compat utf8-string
  ];
  testToolDepends = [ cabal-install ];
  doCheck = false;
  description = "Simple interface to some of Cabal's configuration state, mainly used by ghc-mod";
  license = stdenv.lib.licenses.agpl3;
}
