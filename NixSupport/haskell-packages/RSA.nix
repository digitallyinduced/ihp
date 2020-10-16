{ mkDerivation, base, binary, bytestring, crypto-api
, crypto-pubkey-types, QuickCheck, SHA, stdenv, tagged
, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "RSA";
  version = "2.4.1";
  sha256 = "0hchsqrxpfw7mqrqwscfy8ig1w2di6w3nxpzi873w0gibv2diibj";
  libraryHaskellDepends = [
    base binary bytestring crypto-api crypto-pubkey-types SHA
  ];
  testHaskellDepends = [
    base binary bytestring crypto-api crypto-pubkey-types QuickCheck
    SHA tagged test-framework test-framework-quickcheck2
  ];
  description = "Implementation of RSA, using the padding schemes of PKCS#1 v2.1.";
  license = stdenv.lib.licenses.bsd3;
}