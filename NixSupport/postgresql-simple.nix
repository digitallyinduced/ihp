{ mkDerivation, aeson, attoparsec, base, base16-bytestring
     , bytestring, bytestring-builder, case-insensitive, containers
     , cryptohash, filepath, hashable, HUnit, postgresql-libpq
     , scientific, tasty, tasty-golden, tasty-hunit, template-haskell
     , text, time_1_9_1, transformers, uuid-types, vector, stdenv
     }:
     mkDerivation {
       pname = "postgresql-simple";
       version = "0.5.3.0";
       sha256 = "0h8f4d09f25p0dlz2mxvgp5b5pr56hnhng3yhb69pmv0dmj7n38y";
       revision = "1";
       editedCabalFile = "1hvry7nhk629yas6nbm3wnyy0bgwh6r8511rmsq9r8xkl7qm76r2";
       libraryHaskellDepends = [
         aeson attoparsec base bytestring bytestring-builder
         case-insensitive containers hashable postgresql-libpq scientific
         template-haskell text time_1_9_1  transformers uuid-types vector
       ];
       testHaskellDepends = [
         aeson base base16-bytestring bytestring containers cryptohash
         filepath HUnit tasty tasty-golden tasty-hunit text time_1_9_1 vector
       ];
       description = "Mid-Level PostgreSQL client library";
       license = stdenv.lib.licenses.bsd3;
     }