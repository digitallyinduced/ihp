{ mkDerivation, base, containers, criterion, deepseq, dependent-map
, dependent-sum, ghc-prim, ghc-typelits-knownnat, hedgehog, hspec
, hspec-hedgehog, lib, primitive, vector, fetchFromGitHub
}:
mkDerivation {
  pname = "typerep-map";
  version = "0.6.0.0";
  libraryHaskellDepends = [
    base containers deepseq ghc-prim primitive vector
  ];
  testHaskellDepends = [
    base ghc-typelits-knownnat hedgehog hspec hspec-hedgehog
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq dependent-map dependent-sum
    ghc-typelits-knownnat
  ];
  doHaddock = false;
  homepage = "https://github.com/kowainik/typerep-map";
  description = "Efficient implementation of a dependent map with types as keys";
  license = lib.licenses.mpl20;
  src = fetchFromGitHub {
    owner = "alaendle";
    repo = "typerep-map";
    rev = "444d377fac1d0941721233737ee65b631ea3d056";
    sha256 = "sha256-BU/BvOI920QTW9MCfQyz+8zTRa0Ayx8i6R3matzRAIs";
  };
}