{ mkDerivation, aeson, attoparsec, base, bytestring
, bytestring-strict-builder, contravariant, criterion, dlist
, hashable, hspec, hspec-discover, iproute, lib, mtl
, postgresql-binary, postgresql-connection-string, postgresql-libpq
, profunctors, QuickCheck, quickcheck-instances, random, rerebase
, scientific, testcontainers-postgresql, text, text-builder, time
, transformers, unordered-containers, uuid, vector, witherable
}:
mkDerivation {
  pname = "hasql";
  version = "1.10.2.3";
  sha256 = "ed8fc6d8e05cb9d05d22a63ad0acefac3795ab9ba517f14c574d9d4168f8afca";
  libraryHaskellDepends = [
    aeson attoparsec base bytestring bytestring-strict-builder
    contravariant dlist hashable iproute mtl postgresql-binary
    postgresql-connection-string postgresql-libpq profunctors
    scientific text text-builder time transformers unordered-containers
    uuid vector witherable
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring contravariant dlist hashable hspec
    iproute mtl postgresql-libpq profunctors QuickCheck
    quickcheck-instances random rerebase scientific
    testcontainers-postgresql text text-builder time transformers
    unordered-containers uuid vector witherable
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ criterion rerebase ];
  homepage = "https://github.com/nikita-volkov/hasql";
  description = "Fast PostgreSQL driver with a flexible mapping API";
  license = lib.licenses.mit;
}
