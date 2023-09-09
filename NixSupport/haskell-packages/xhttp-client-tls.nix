{ mkDerivation, base, bytestring, case-insensitive, containers
, crypton, crypton-connection, data-default-class, exceptions
, gauge, hspec, http-client, http-types, lib, memory, network
, network-uri, text, tls, transformers
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.6.3";
  sha256 = "15chmlnq0nk2kxhk65r9xdjkzl94k0axcml89a5qkdiffwyzrp1q";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers crypton
    crypton-connection data-default-class exceptions http-client
    http-types memory network network-uri text tls transformers
  ];
  testHaskellDepends = [
    base crypton-connection hspec http-client http-types
  ];
  benchmarkHaskellDepends = [ base gauge http-client ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = lib.licenses.mit;
}