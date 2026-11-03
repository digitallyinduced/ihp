{ mkDerivation, base, bytestring, hasql, hasql-pool, hspec
, hspec-discover, lib, pqi, pqi-ffi, QuickCheck, source, text
}:
mkDerivation {
  pname = "hasql-notifications";
  version = "0.2.5.0";
  src = source;
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
