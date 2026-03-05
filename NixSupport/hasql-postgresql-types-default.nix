{ mkDerivation, base, fetchgit, hasql, hasql-mapping, lib
, postgresql-types, postgresql-types-algebra, ptr-peeker, ptr-poker
, tagged, text-builder
}:
mkDerivation {
  pname = "hasql-postgresql-types";
  version = "0.1.0.1";
  src = fetchgit {
    url = "https://github.com/nikita-volkov/hasql-postgresql-types";
    sha256 = "0fffxiavxn70nis9rqgx2z9rp030x1afdr7qj8plwncif3qvsv1f";
    rev = "b8cb8fe1e7ebf76dc03ecdc78990939aca09d429";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base hasql hasql-mapping postgresql-types postgresql-types-algebra
    ptr-peeker ptr-poker tagged text-builder
  ];
  homepage = "https://github.com/nikita-volkov/hasql-postgresql-types";
  description = "Integration of \"hasql\" with \"postgresql-types\"";
  license = lib.licenses.mit;
}
