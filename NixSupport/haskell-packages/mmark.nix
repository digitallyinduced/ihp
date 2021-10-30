{ mkDerivation, aeson, base, case-insensitive, containers
, criterion, deepseq, dlist, email-validate, foldl, hashable, hspec
, hspec-megaparsec, html-entity-map, lib, lucid, megaparsec
, microlens, microlens-th, modern-uri, mtl, parser-combinators
, QuickCheck, text, text-metrics, unordered-containers, weigh, yaml
, fetchFromGitHub
}:
mkDerivation {
  pname = "mmark";
  version = "0.0.7.2";
  sha256 = "b59e3b2502b14d1304953593febb9a16d408b5fa4dfc8249066f3ba3d6ff9af3";
  revision = "3";
  editedCabalFile = "1ffa76pz544pa3s764lnc38rdmfccyn8z6zn1w76pqb01p0f9k9p";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base case-insensitive containers deepseq dlist email-validate
    foldl hashable html-entity-map lucid megaparsec microlens
    microlens-th modern-uri mtl parser-combinators text text-metrics
    unordered-containers yaml
  ];
  testHaskellDepends = [
    aeson base foldl hspec hspec-megaparsec lucid megaparsec modern-uri
    QuickCheck text
  ];
  benchmarkHaskellDepends = [ base criterion text weigh ];
  homepage = "https://github.com/mmark-md/mmark";
  description = "Strict markdown processor for writers";
  license = lib.licenses.bsd3;

  src = fetchFromGitHub {
    owner = "mmark-md";
    repo = "mmark";
    rev = "42b13844f41e7c8b950cfe6ba23632bdca9df0a2";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i5";
  };
}