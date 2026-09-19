{ mkDerivation, base, bytestring, hspec, lib, postgresql-libpq, pqi
, pqi-conformance
}:
mkDerivation {
  pname = "pqi-ffi";
  version = "1.0.1.0";
  sha256 = "18484d37ab84cba34607091f296eed14fed531d1d0eddbec386fd1b94f0e4a7e";
  libraryHaskellDepends = [ base bytestring postgresql-libpq pqi ];
  testHaskellDepends = [ base hspec pqi-conformance ];
  homepage = "https://github.com/nikita-volkov/pqi-ffi";
  description = "FFI adapter for pqi, backed by postgresql-libpq";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
