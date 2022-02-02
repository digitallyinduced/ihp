{ mkDerivation, base, bytestring, fetchzip, gauge, lib, primitive
, primitive-addr, primitive-unlifted, quickcheck-classes, run-st
, tasty, tasty-hunit, tasty-quickcheck, transformers, tuples
, vector
}:
mkDerivation {
  pname = "byteslice";
  version = "0.2.7.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/byteslice/archive/refs/heads/master.zip";
    sha256 = "0i0zhljidw5sgvckb3n8nxkhppyk6y8p22pr74slzx2gpnn4ckcv";
  };
  libraryHaskellDepends = [
    base bytestring primitive primitive-addr primitive-unlifted run-st
    tuples vector
  ];
  testHaskellDepends = [
    base bytestring primitive quickcheck-classes tasty tasty-hunit
    tasty-quickcheck transformers
  ];
  benchmarkHaskellDepends = [ base gauge primitive ];
  homepage = "https://github.com/andrewthad/byteslice";
  description = "Slicing managed and unmanaged memory";
  license = lib.licenses.bsd3;
}
