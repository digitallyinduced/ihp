{ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, lib
, text
}:
mkDerivation {
  pname = "ghc-syntax-highlighter";
  version = "0.0.9.0";
  sha256 = "0dan000fg9ipfh8knhrdscnr8lvcf6p2djl9b4bgqd4nc65pcf8a";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ghc-lib-parser text ];
  testHaskellDepends = [ base hspec text ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mrkkrp/ghc-syntax-highlighter";
  description = "Syntax highlighter for Haskell using the lexer of GHC";
  license = lib.licenses.bsd3;
}