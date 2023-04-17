{ mkDerivation, aeson, base, bound, elm-syntax, generics-sop, lib
, text, time, unordered-containers, vector, fetchFromGitHub
}:
mkDerivation {
  pname = "haskell-to-elm";
  version = "0.3.2.0";
  sha256 = "sha256-cFSn5d0eWWCycusL2ZYk8CMzivJplp/YP8HJx0X1ROM";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bound elm-syntax generics-sop text time
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bound elm-syntax generics-sop text time
    unordered-containers vector
  ];
  homepage = "https://github.com/folq/haskell-to-elm#readme";
  description = "Generate Elm types and JSON encoders and decoders from Haskell types";
  license = lib.licenses.bsd3;
  src = fetchFromGitHub {
    owner = "SupercedeTech";
    repo = "haskell-to-elm";
    rev = "ghc943-workaround";
    sha256 = "sha256-cFSn5d0eWWCycusL2ZYk8CMzivJplp/YP8HJx0X1ROM=";
  };
}