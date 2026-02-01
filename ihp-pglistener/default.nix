{ mkDerivation, aeson, async, base, bytestring, containers
, hashable, hasql, hasql-notifications, hspec, ihp-log, lib
, safe-exceptions, string-conversions, text, unagi-chan
, unordered-containers, uuid
}:
mkDerivation {
  pname = "ihp-pglistener";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers hashable hasql
    hasql-notifications ihp-log safe-exceptions string-conversions text
    unagi-chan unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson async base bytestring containers hashable hasql
    hasql-notifications hspec ihp-log safe-exceptions
    string-conversions text unagi-chan unordered-containers uuid
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "PostgreSQL LISTEN/NOTIFY channel manager for IHP";
  license = lib.licenses.mit;
}
