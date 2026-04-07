{ mkDerivation, aeson, base, binary-parser, bytestring
, bytestring-strict-builder, containers, criterion, iproute, lib
, mtl, postgresql-libpq, QuickCheck, quickcheck-instances, rerebase
, scientific, tasty, tasty-hunit, tasty-quickcheck, text, time
, transformers, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "postgresql-binary";
  version = "0.15.0.1";
  sha256 = "7736e091b51820526a6417ab9932c083ffd31f0d3ec5baea1d5a07b65d85e34f";
  libraryHaskellDepends = [
    aeson base binary-parser bytestring bytestring-strict-builder
    containers iproute mtl scientific text time transformers
    unordered-containers uuid vector
  ];
  testHaskellDepends = [
    aeson iproute postgresql-libpq QuickCheck quickcheck-instances
    rerebase tasty tasty-hunit tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ criterion rerebase ];
  homepage = "https://github.com/nikita-volkov/postgresql-binary";
  description = "Encoders and decoders for the PostgreSQL's binary format";
  license = lib.licenses.mit;
}
