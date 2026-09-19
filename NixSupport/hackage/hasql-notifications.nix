{ mkDerivation, base, bytestring, hasql, hasql-pool, hspec
, hspec-discover, lib, pqi, pqi-ffi, QuickCheck, text
}:
mkDerivation {
  pname = "hasql-notifications";
  version = "0.2.6.0";
  sha256 = "bee28f6f6e626ab004c09e743227d596b9a01a59fdb84f947cc1400e914e4977";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring hasql hasql-pool pqi text
  ];
  executableHaskellDepends = [ base hasql pqi-ffi ];
  testHaskellDepends = [
    base bytestring hasql hspec pqi-ffi QuickCheck
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/diogob/hasql-notifications";
  description = "LISTEN/NOTIFY support for Hasql";
  license = lib.licenses.bsd3;
  mainProgram = "hasql-notifications";
}
