{ mkDerivation, aeson, base, base64-bytestring, binary, bytestring
, case-insensitive, conduit, conduit-extra, connection, cryptonite
, cryptonite-conduit, digest, directory, exceptions, filepath
, http-client, http-client-tls, http-conduit, http-types, ini
, memory, network-uri, protolude, QuickCheck, raw-strings-qq
, resourcet, retry, lib, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, text, time, transformers, unliftio
, unliftio-core, unordered-containers, xml-conduit, fetchFromGitHub
}:
mkDerivation {
  pname = "minio-hs";
  version = "1.5.3";
  src = fetchFromGitHub {
    owner = "minio";
    repo = "minio-hs";
    rev = "c52f2811fe8eb2a657f1467d0798067a15deb395";
    sha256 = "13xm932qqdl9qrj3q8p5ad5djs4wbvbkq8h87v54d5yjsf8pmh5y";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring binary bytestring case-insensitive
    conduit conduit-extra connection cryptonite cryptonite-conduit
    digest directory exceptions filepath http-client http-client-tls
    http-conduit http-types ini memory network-uri protolude
    raw-strings-qq resourcet retry text time transformers unliftio
    unliftio-core unordered-containers xml-conduit
  ];
  testHaskellDepends = [
    aeson base base64-bytestring binary bytestring case-insensitive
    conduit conduit-extra connection cryptonite cryptonite-conduit
    digest directory exceptions filepath http-client http-client-tls
    http-conduit http-types ini memory network-uri protolude QuickCheck
    raw-strings-qq resourcet retry tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck text time transformers unliftio unliftio-core
    unordered-containers xml-conduit
  ];
  homepage = "https://github.com/minio/minio-hs#readme";
  description = "A MinIO Haskell Library for Amazon S3 compatible cloud storage";
  license = lib.licenses.asl20;
}