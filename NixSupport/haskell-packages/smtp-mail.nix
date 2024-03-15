{ mkDerivation, array, base, base16-bytestring, base64-bytestring
, bytestring, crypton-connection, cryptonite, filepath, lib, memory
, mime-mail, network, network-bsd, text, fetchFromGitHub
}:
mkDerivation {
  pname = "smtp-mail";
  version = "0.3.0.0";
  src = fetchFromGitHub {
    owner = "MasterWordServices";
    repo = "smtp-mail";
    rev = "4c724c80814ab1da7c37256a6c10e04c88b9af95";
    sha256 = "sha256-gjg2k8UHBwumoH8OUsFZ+Xd5icotaVHRmlQt6Xc7Mf0=";
  };
  libraryHaskellDepends = [
    array base base16-bytestring base64-bytestring bytestring
    crypton-connection cryptonite filepath memory mime-mail network network-bsd
    text
  ];
  homepage = "http://github.com/jhickner/smtp-mail";
  description = "Simple email sending via SMTP";
  license = lib.licenses.bsd3;
}