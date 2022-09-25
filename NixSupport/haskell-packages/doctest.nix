{ mkDerivation, base, base-compat, code-page, deepseq, directory
, exceptions, filepath, ghc, ghc-paths, hspec, hspec-core
, hspec-discover, HUnit, lib, mockery, process, QuickCheck, setenv
, silently, stringbuilder, syb, transformers, fetchFromGitHub
}:
mkDerivation {
  pname = "doctest";
  version = "0.20.0";
  sha256 = "0sk50b8zxq4hvc8qphlmfha1lsv3xha7q7ka081jgswf1qpg34y4";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat code-page deepseq directory exceptions filepath
    ghc ghc-paths process syb transformers
  ];
  executableHaskellDepends = [
    base base-compat code-page deepseq directory exceptions filepath
    ghc ghc-paths process syb transformers
  ];
  testHaskellDepends = [
    base base-compat code-page deepseq directory exceptions filepath
    ghc ghc-paths hspec hspec-core HUnit mockery process QuickCheck
    setenv silently stringbuilder syb transformers
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/sol/doctest#readme";
  description = "Test interactive Haskell examples";
  license = lib.licenses.mit;
  mainProgram = "doctest";
  src = fetchFromGitHub {
    owner = "kazu-yamamoto";
    repo = "doctest";
    rev = "ghc94";
    sha256 = "sha256-DUPnRB88ZOCIAO9Ve80Oq4kBtfrqIOvaHQItcgrrtVU";
  };
}