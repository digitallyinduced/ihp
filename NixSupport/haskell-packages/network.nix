{ mkDerivation, base, bytestring, deepseq, directory, hspec
, hspec-discover, HUnit, stdenv, unix
}:
mkDerivation {
  pname = "network";
  version = "3.0.0.1";
  sha256 = "03f7gi3skz2ivack73wgn0zsppxwscl6j6xvwjal6i7y3rzajiam";
  libraryHaskellDepends = [ base bytestring deepseq unix ];
  testHaskellDepends = [ base bytestring directory hspec HUnit ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/haskell/network";
  description = "Low-level networking interface";
  license = stdenv.lib.licenses.bsd3;
}