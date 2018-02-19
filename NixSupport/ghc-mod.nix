
{ mkDerivation, base, binary, bytestring, Cabal, cabal-helper
, containers, criterion, deepseq, directory, djinn-ghc, doctest
, extra, fclabels, fetchgit, filepath, ghc, ghc-boot, ghc-paths
, ghc-syb-utils, haskell-src-exts, hlint, hspec, monad-control
, monad-journal, mtl, old-time, optparse-applicative, pipes
, process, safe, semigroups, split, stdenv, syb, template-haskell
, temporary, text, time, transformers, transformers-base, cabal-doctest
, pkgs
}:
mkDerivation {
  pname = "ghc-mod";
  version = "5.9.0.0";
  src = pkgs.fetchFromGitHub {
    owner = "DanielG";
    repo = "ghc-mod";
    rev = "6bf0ac7de82a28f20e4144ff2ca66ed97b8fee03";
    sha256 = "1vfdp30hcvi6yh97rk6p387vm1yzxk2bqxb9r9l9d6cdncnw2b00";
  };

  # src = fetchgit {
  #   url = "https://github.com/DanielG/ghc-mod/";
  #   sha256 = "0f9qzk3czamqjb42xg2bmx70hafza8cn84zylx60bw8yx4i0q7nx";
  #   rev = "39b96c475090f91e4f717197c96e083fdb2ccaf7";
  # };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [
    base Cabal containers directory filepath process template-haskell
    transformers cabal-doctest
  ];
  libraryHaskellDepends = [
    base binary bytestring cabal-helper containers deepseq directory
    djinn-ghc extra fclabels filepath ghc ghc-boot ghc-paths
    ghc-syb-utils haskell-src-exts hlint monad-control monad-journal
    mtl old-time optparse-applicative pipes process safe semigroups
    split syb template-haskell temporary text time transformers
    transformers-base
  ];
  executableHaskellDepends = [
    base binary deepseq directory fclabels filepath ghc monad-control
    mtl old-time optparse-applicative process semigroups split time
  ];
  testHaskellDepends = [
    base cabal-helper containers directory doctest fclabels filepath
    ghc ghc-boot hspec monad-journal mtl process split temporary
    transformers
  ];
  benchmarkHaskellDepends = [
    base criterion directory filepath temporary
  ];
  homepage = "https://github.com/DanielG/ghc-mod";
  description = "Happy Haskell Hacking";
  license = stdenv.lib.licenses.agpl3;
}
