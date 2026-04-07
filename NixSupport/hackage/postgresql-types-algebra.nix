{ mkDerivation, attoparsec, base, bytestring, lib, ptr-peeker
, ptr-poker, tagged, text, text-builder
}:
mkDerivation {
  pname = "postgresql-types-algebra";
  version = "0.1";
  sha256 = "6c40ca9d442f0b6c01f08b3d3e184c29674223b445edef4a3e672ec3ec193ae4";
  libraryHaskellDepends = [
    attoparsec base bytestring ptr-peeker ptr-poker tagged text
    text-builder
  ];
  homepage = "https://github.com/nikita-volkov/postgresql-types-algebra";
  description = "Type classes for PostgreSQL type mappings";
  license = lib.licenses.mit;
}
