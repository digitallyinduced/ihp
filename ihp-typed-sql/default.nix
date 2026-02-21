{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, haskell-src-meta, hasql, hasql-dynamic-statements
, hasql-pool, hspec, ihp, lib
, postgresql-libpq, postgresql-syntax, postgresql-types, process
, scientific, string-conversions, template-haskell, temporary, text
}:
mkDerivation {
  pname = "ihp-typed-sql";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers haskell-src-meta hasql
    hasql-dynamic-statements hasql-pool ihp
    postgresql-libpq postgresql-syntax postgresql-types scientific
    string-conversions template-haskell text
  ];
  testHaskellDepends = [
    base directory filepath hspec ihp process string-conversions
    temporary text
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Compile-time typed SQL quasiquoter for IHP";
  license = lib.licenses.mit;
}
