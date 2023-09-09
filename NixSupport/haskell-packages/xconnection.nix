{ mkDerivation, base, basement, bytestring, containers
, data-default-class, lib, network, socks, tls, x509, x509-store
, x509-system, x509-validation, fetchFromGitHub
, crypton-x509-system
, crypton-x509-validation
, crypton-x509-store
, crypton-x509
}:
mkDerivation {
  pname = "connection";
  version = "0.4.1";
  libraryHaskellDepends = [
    base basement bytestring containers data-default-class network
    socks tls x509 x509-store x509-system x509-validation
    crypton-x509-system
    crypton-x509-validation
    crypton-x509-store
    crypton-x509
  ];
  homepage = "https://github.com/vincenthz/hs-connection";
  description = "Simple and easy network connections API";
  license = lib.licenses.bsd3;
  src = fetchFromGitHub {
    owner = "vincenthz";
    repo = "hs-connection";
    rev = "1dc000d38963de0e4c3954b27d4fe0e5f1d9cee2";
    sha256 = "sha256-yyKmEYMdCmicz3jSe1UOD/t3Bsgukb9pWeY0NZPP39Q";
  };
}