{ mkDerivation, base, deepseq, fetchzip, lib, primitive
, primitive-unlifted, QuickCheck, quickcheck-classes
, quickcheck-instances, random, random-shuffle, run-st, vector
, weigh
}:
mkDerivation {
  pname = "contiguous";
  version = "0.6.1.1";
  src = fetchzip {
    url = "https://github.com/andrewthad/contiguous/archive/refs/heads/master.zip";
    sha256 = "1rvqra1bkhi5rm69kai0lgr09glr6igpdrvla14pqcsrbh0lka15";
  };
  libraryHaskellDepends = [
    base deepseq primitive primitive-unlifted run-st
  ];
  testHaskellDepends = [
    base primitive QuickCheck quickcheck-classes quickcheck-instances
    vector
  ];
  benchmarkHaskellDepends = [
    base primitive random random-shuffle weigh
  ];
  homepage = "https://github.com/andrewthad/contiguous";
  description = "Unified interface for primitive arrays";
  license = lib.licenses.bsd3;
}
