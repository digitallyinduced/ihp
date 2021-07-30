{ mkDerivation, aeson, base, case-insensitive, containers
, criterion, deepseq, dlist, email-validate, foldl, hashable, hspec
, hspec-megaparsec, html-entity-map, lucid, megaparsec, microlens
, microlens-th, modern-uri, mtl, parser-combinators, QuickCheck
, lib, text, text-metrics, unordered-containers, weigh, yaml, fetchFromGitHub
}:
mkDerivation {
  pname = "mmark";
  version = "0.0.7.3";
  sha256 = "1gfl9jhqm1jaqxi0yxd8r4z3ai5c3f1wv53vjs0ln84qjpcqp30s";
  src = fetchFromGitHub {
    owner = "mmark-md";
    repo = "mmark";
    rev = "dced8600ea267f8a2bb298789b98866202cf44eb";
    sha256 = "1l9k7g915zi567ymhxkq90k9b5072krha8lzb7zhnw8bsw1pfh42";
  };
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
  doCheck = true;
  jailbreak = true;
}