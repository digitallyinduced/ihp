{ mkDerivation, base, bytestring, hasql, hasql-pool, hspec
, hspec-discover, lib, postgresql-libpq, QuickCheck, text
}:
mkDerivation {
  pname = "hasql-notifications";
  version = "0.2.5.0";
  sha256 = "4bc7a81a5831d6a39e67baa1bab5d79a05e4444423a35ce93879971d00a46f7e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring hasql hasql-pool postgresql-libpq text
  ];
  executableHaskellDepends = [ base hasql ];
  testHaskellDepends = [ base bytestring hasql hspec QuickCheck ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/diogob/hasql-notifications";
  description = "LISTEN/NOTIFY support for Hasql";
  license = lib.licenses.bsd3;
  mainProgram = "hasql-notifications";
}
