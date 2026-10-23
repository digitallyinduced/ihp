{ mkDerivation, aeson, async, base, bytestring, containers
, fast-logger, hashable, hasql, hasql-notifications, hspec, lib
, pqi-ffi, pqi-native, safe-exceptions, string-conversions, text
, unagi-chan, unordered-containers, uuid
}:
mkDerivation {
  pname = "ihp-pglistener";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring containers fast-logger hashable hasql
    hasql-notifications pqi-ffi safe-exceptions string-conversions text
    unagi-chan unordered-containers uuid
  ];
  testHaskellDepends = [
    aeson async base bytestring containers fast-logger hashable hasql
    hasql-notifications hspec pqi-ffi pqi-native safe-exceptions
    string-conversions text unagi-chan unordered-containers uuid
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "PostgreSQL LISTEN/NOTIFY channel manager for IHP";
  license = lib.licenses.mit;
}
