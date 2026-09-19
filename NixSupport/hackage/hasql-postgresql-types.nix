{ mkDerivation, base, hasql, hasql-mapping, lib, postgresql-types
, postgresql-types-algebra, ptr-peeker, ptr-poker, tagged
, text-builder
}:
mkDerivation {
  pname = "hasql-postgresql-types";
  version = "0.2.1.3";
  sha256 = "18a9112ef266d3283a9aa1109121bee10327a8adc6273e56d56dad4523296b7a";
  libraryHaskellDepends = [
    base hasql hasql-mapping postgresql-types postgresql-types-algebra
    ptr-peeker ptr-poker tagged text-builder
  ];
  homepage = "https://github.com/nikita-volkov/hasql-postgresql-types";
  description = "Integration of \"hasql\" with \"postgresql-types\"";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
