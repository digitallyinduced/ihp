{ mkDerivation, aeson, base, bytestring, containers, directory
, dlist, exceptions, filepath, ghc-lib-parser, gitrev, hspec
, hspec-discover, HsYAML, HsYAML-aeson, mtl, optparse-applicative
, path, path-io, stdenv, syb, text
}:
mkDerivation {
  pname = "fourmolu";
  version = "0.2.0.0";
  sha256 = "1jak0xgd6gcbw7icyrblvqnvzjyyakpw2zfnqj1z958qyg763v52";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory dlist exceptions
    filepath ghc-lib-parser HsYAML HsYAML-aeson mtl syb text
  ];
  executableHaskellDepends = [
    base directory ghc-lib-parser gitrev optparse-applicative text
  ];
  testHaskellDepends = [
    base containers filepath hspec path path-io text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/parsonsmatt/fourmolu";
  description = "A formatter for Haskell source code";
  license = stdenv.lib.licenses.bsd3;
}