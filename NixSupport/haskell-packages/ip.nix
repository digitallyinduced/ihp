{ mkDerivation, aeson, attoparsec, base, bytebuild, byteslice
, bytesmith, bytestring, criterion, deepseq, doctest, fetchzip
, hashable, hspec, hspec-discover, HUnit, lib, natural-arithmetic
, primitive, QuickCheck, quickcheck-classes, random, tasty
, tasty-hunit, tasty-quickcheck, text, text-short, vector
, wide-word
}:
mkDerivation {
  pname = "ip";
  version = "1.7.4";
  src = fetchzip {
    url = "https://github.com/zacwood9/haskell-ip/archive/refs/heads/master.zip";
    sha256 = "0x73191zhn6i6ll1w7v2q3y62aw2h30nwkj08f8jyipfq9naj9ya";
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytebuild byteslice bytesmith bytestring
    deepseq hashable natural-arithmetic primitive text text-short
    vector wide-word
  ];
  testHaskellDepends = [
    attoparsec base byteslice bytestring doctest hspec HUnit QuickCheck
    quickcheck-classes tasty tasty-hunit tasty-quickcheck text
    text-short vector wide-word
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [
    attoparsec base byteslice bytestring criterion primitive random
    text
  ];
  homepage = "https://github.com/andrewthad/haskell-ip#readme";
  description = "Library for IP and MAC addresses";
  doCheck = false;
  license = lib.licenses.bsd3;
}
