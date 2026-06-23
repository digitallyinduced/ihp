{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, haskell-src-meta, hasql, hasql-dynamic-statements
, hasql-mapping, hasql-pool, hspec, ihp, fast-logger, lib
, postgresql-libpq, postgresql-syntax, postgresql-types, process
, scientific, string-conversions, template-haskell, temporary
, temporary-ospath, text, wai
}:
mkDerivation {
  pname = "ihp-typed-sql";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath haskell-src-meta hasql
    hasql-dynamic-statements hasql-mapping hasql-pool ihp
    postgresql-libpq postgresql-syntax postgresql-types process scientific
    string-conversions template-haskell temporary text wai
  ];
  testHaskellDepends = [
    base containers directory filepath hspec ihp fast-logger process
    string-conversions temporary-ospath text
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Compile-time typed SQL quasiquoter for IHP";
  license = lib.licenses.mit;
}
