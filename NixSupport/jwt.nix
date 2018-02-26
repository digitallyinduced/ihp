{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, data-default, doctest, HsOpenSSL, http-types, HUnit, lens
, lens-aeson, memory, network-uri, QuickCheck, RSA, scientific
, semigroups, stdenv, tasty, tasty-hunit, tasty-quickcheck
, tasty-th, text, time, unordered-containers, vector, fetchgit
}:
mkDerivation {
  pname = "jwt";
  version = "0.7.2";
  src = fetchgit {
    url = "https://bitbucket.org/pbrisbin/haskell-jwt.git";
    sha256 = "1x1apv506laiwwv6rs9ji8wbw5139r10bchfd49zf2gwkrclb6ky";
    rev = "202898d8aa20ab4d4fdff3a9efb08a973ecd0bc4";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite data-default HsOpenSSL
    http-types memory network-uri RSA scientific semigroups text time
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers cryptonite data-default doctest
    HsOpenSSL http-types HUnit lens lens-aeson memory network-uri
    QuickCheck RSA scientific semigroups tasty tasty-hunit
    tasty-quickcheck tasty-th text time unordered-containers vector
  ];
  homepage = "https://bitbucket.org/ssaasen/haskell-jwt";
  description = "JSON Web Token (JWT) decoding and encoding";
  license = stdenv.lib.licenses.mit;
}