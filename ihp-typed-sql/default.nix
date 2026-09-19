{ mkDerivation, aeson, base, bytestring, containers, countable-inflections
, deepseq, directory, filepath, hashable, haskell-src-meta, hasql
, hasql-dynamic-statements
, hasql-implicits, hasql-mapping, hasql-pool, hasql-postgresql-types, hspec
, inflections, lib, postgresql-libpq, postgresql-syntax, postgresql-types
, pqi-ffi
, process, scientific, string-conversions, template-haskell, temporary
, temporary-ospath, text, time, unix, uuid
}:
mkDerivation {
  pname = "ihp-typed-sql";
  version = "1.7.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers countable-inflections deepseq directory
    filepath hashable haskell-src-meta hasql hasql-dynamic-statements
    hasql-implicits
    hasql-mapping hasql-pool hasql-postgresql-types inflections postgresql-libpq
    postgresql-syntax
    postgresql-types pqi-ffi process scientific string-conversions template-haskell
    temporary text time unix uuid
  ];
  testHaskellDepends = [
    base containers directory filepath hasql hasql-pool hspec pqi-ffi process
    string-conversions temporary-ospath text unix
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Compile-time typed SQL quasiquoter for IHP";
  license = lib.licenses.mit;
}
