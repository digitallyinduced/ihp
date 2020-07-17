{ mkDerivation, base, bytestring, containers, deepseq, HUnit
, network, network-bsd, old-locale, stdenv, time, unix
}:
mkDerivation {
  pname = "hslogger";
  version = "1.3.1.0";
  sha256 = "0nyar9xcblx5jwks85y8f4jfy9k1h4ss6rvj4mdbiidrq3v688vz";
  libraryHaskellDepends = [
    base bytestring containers deepseq network network-bsd old-locale
    time unix
  ];
  testHaskellDepends = [ base HUnit ];
  homepage = "https://github.com/hvr/hslogger/wiki";
  description = "Versatile logging framework";
  license = stdenv.lib.licenses.bsd3;
}