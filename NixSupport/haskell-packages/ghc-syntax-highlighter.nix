{ mkDerivation, base, ghc-lib-parser, hspec, hspec-discover, lib
, text
, fetchFromGitHub
}:
mkDerivation {
  pname = "ghc-syntax-highlighter";
  version = "0.0.6.0";
  sha256 = "4a2ab2a8a4a6c8536bf2aba0823bfd7fdb41ebae7a47423975690c4f0827b5b7";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base ghc-lib-parser text ];
  testHaskellDepends = [ base hspec text ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/mrkkrp/ghc-syntax-highlighter";
  description = "Syntax highlighter for Haskell using lexer of GHC itself";
  license = lib.licenses.bsd3;
  
  # https://github.com/mrkkrp/ghc-syntax-highlighter/pull/24
  src = fetchFromGitHub {
    owner = "amesgen";
    repo = "ghc-syntax-highlighter";
    rev = "18fd807f5b632ab03598c3a989622de0dcc82b1d";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i7";
  };
}