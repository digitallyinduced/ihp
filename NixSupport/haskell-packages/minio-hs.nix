{ mkDerivation, aeson, base, base64-bytestring, binary, bytestring
, case-insensitive, conduit, conduit-extra, crypton-connection
, cryptonite, cryptonite-conduit, digest, directory, filepath
, http-client, http-client-tls, http-conduit, http-types, ini, lib
, memory, network-uri, QuickCheck, raw-strings-qq, relude
, resourcet, retry, tasty, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, text, time, time-units, transformers, unliftio
, unliftio-core, unordered-containers, xml-conduit, fetchFromGitHub
}:
mkDerivation {
  pname = "minio-hs";
  version = "1.7.0";
  src = fetchFromGitHub {
    owner = "mpscholten";
    repo = "minio-hs";
    rev = "786cf1881f0b62b7539e63547e76afc3c1ade36a";
    sha256 = "sha256-dK4nH6j16oFNB27tp1ExAWahhFUlKpzLpqTiHju8dl8=";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring binary bytestring case-insensitive
    conduit conduit-extra crypton-connection cryptonite
    cryptonite-conduit digest directory filepath http-client
    http-client-tls http-conduit http-types ini memory network-uri
    relude resourcet retry text time time-units transformers unliftio
    unliftio-core unordered-containers xml-conduit
  ];
  testHaskellDepends = [
    aeson base base64-bytestring binary bytestring case-insensitive
    conduit conduit-extra crypton-connection cryptonite
    cryptonite-conduit digest directory filepath http-client
    http-client-tls http-conduit http-types ini memory network-uri
    QuickCheck raw-strings-qq relude resourcet retry tasty tasty-hunit
    tasty-quickcheck tasty-smallcheck text time time-units transformers
    unliftio unliftio-core unordered-containers xml-conduit
  ];
  homepage = "https://github.com/minio/minio-hs#readme";
  description = "A MinIO Haskell Library for Amazon S3 compatible cloud storage";
  license = lib.licenses.asl20;
}