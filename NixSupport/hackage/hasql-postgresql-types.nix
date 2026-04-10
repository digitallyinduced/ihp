{ mkDerivation, base, hasql, hasql-mapping, lib, postgresql-types
, postgresql-types-algebra, ptr-peeker, ptr-poker, tagged
, text-builder
}:
mkDerivation {
  pname = "hasql-postgresql-types";
  version = "0.2";
  sha256 = "54d4c9bc063a030753d2aea7b2ff6c6a65cbdeafe50df3dfd1879f946915d083";
  libraryHaskellDepends = [
    base hasql hasql-mapping postgresql-types postgresql-types-algebra
    ptr-peeker ptr-poker tagged text-builder
  ];
  homepage = "https://github.com/nikita-volkov/hasql-postgresql-types";
  description = "Integration of \"hasql\" with \"postgresql-types\"";
  license = lib.licenses.mit;
}
