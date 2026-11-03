{ mkDerivation, base, bytestring, lib, source, text }:
mkDerivation {
  pname = "pqi";
  version = "1.1.0.1";
  src = source;
  libraryHaskellDepends = [ base bytestring text ];
  homepage = "https://github.com/nikita-volkov/pqi";
  description = "Driver-agnostic interface to the PostgreSQL libpq API";
  license = lib.licenses.mit;
}
