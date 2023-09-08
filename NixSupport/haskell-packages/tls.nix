{ mkDerivation, asn1-encoding, asn1-types, async, base, bytestring
, cereal, crypton, crypton-x509, crypton-x509-store
, crypton-x509-validation, data-default-class, gauge, hourglass
, lib, memory, mtl, network, QuickCheck, tasty, tasty-quickcheck
, transformers, unix-time
}:
mkDerivation {
  pname = "tls";
  version = "1.7.1";
  sha256 = "0q277cp2dqp5cs7mmzqavi8sm0km7xbf2zgcqjp17491w7q5a687";
  libraryHaskellDepends = [
    asn1-encoding asn1-types async base bytestring cereal crypton
    crypton-x509 crypton-x509-store crypton-x509-validation
    data-default-class memory mtl network transformers unix-time
  ];
  testHaskellDepends = [
    asn1-types async base bytestring crypton crypton-x509
    crypton-x509-validation data-default-class hourglass QuickCheck
    tasty tasty-quickcheck
  ];
  benchmarkHaskellDepends = [
    asn1-types async base bytestring crypton crypton-x509
    crypton-x509-validation data-default-class gauge hourglass
    QuickCheck tasty-quickcheck
  ];
  homepage = "http://github.com/vincenthz/hs-tls";
  description = "TLS/SSL protocol native implementation (Server and Client)";
  license = lib.licenses.bsd3;
}