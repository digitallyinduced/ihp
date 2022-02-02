{ mkDerivation, base, byte-order, byteslice, bytestring, contiguous
, fetchzip, gauge, lib, primitive, run-st, tasty, tasty-hunit
, tasty-quickcheck, text-short, wide-word
}:
mkDerivation {
  pname = "bytesmith";
  version = "0.3.8.0";
  src = fetchzip {
    url = "https://github.com/zacwood9/bytesmith/archive/refs/heads/master.zip";
    sha256 = "146shhsdajv4ijprbks0hh9zbn6fll4chfc9b1svnvgarm1g2pd7";
  };
  libraryHaskellDepends = [
    base byteslice bytestring contiguous primitive run-st text-short
    wide-word
  ];
  testHaskellDepends = [
    base byte-order byteslice primitive tasty tasty-hunit
    tasty-quickcheck text-short wide-word
  ];
  benchmarkHaskellDepends = [
    base byteslice bytestring gauge primitive
  ];
  homepage = "https://github.com/andrewthad/bytesmith";
  description = "Nonresumable byte parser";
  license = lib.licenses.bsd3;
}
