{ mkDerivation, aeson, async, base, bytestring, containers
, contravariant, countable-inflections, deepseq, directory
, filepath, hashable, haskell-src-meta, hasql
, hasql-dynamic-statements, hasql-implicits, hasql-mapping
, hasql-pool, hasql-postgresql-types, hasql-transaction, hspec
, inflections, lib, postgresql-libpq, postgresql-syntax
, postgresql-types, pqi-ffi, process, scientific
, string-conversions, template-haskell, temporary, temporary-ospath
, text, time, unix, uuid, vector
}:
mkDerivation {
  pname = "ihp-typed-sql";
  version = "1.7.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers contravariant
    countable-inflections deepseq directory filepath hashable
    haskell-src-meta hasql hasql-dynamic-statements hasql-implicits
    hasql-mapping hasql-pool hasql-postgresql-types inflections
    postgresql-libpq postgresql-syntax postgresql-types pqi-ffi
    process scientific string-conversions template-haskell temporary
    text time unix uuid vector
  ];
  testHaskellDepends = [
    async base bytestring containers directory filepath hasql
    hasql-dynamic-statements hasql-implicits hasql-mapping hasql-pool
    hasql-postgresql-types hasql-transaction hspec pqi-ffi process
    string-conversions temporary temporary-ospath text unix vector
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Compile-time typed SQL quasiquoter for IHP";
  license = lib.licenses.mit;
}
