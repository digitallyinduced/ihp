{ mkDerivation, base, binary, bytestring, deepseq, ghc-prim
, hashable, lib, quickcheck-instances, tasty, tasty-hunit
, tasty-quickcheck, text
, fetchFromGitHub
}:
mkDerivation {
  pname = "text-short";
  version = "0.1.3";
  sha256 = "bf5b9fedb7d0301e8fdf33e3223d10ca940e9e72c18bac135be80b6016edd977";
  revision = "2";
  editedCabalFile = "17cb7p0qywf2dsrq3g8qb3ssknd9wl5k0nc2pxz9gc3l8rxpkw51";
  libraryHaskellDepends = [
    base binary bytestring deepseq ghc-prim hashable text
  ];
  testHaskellDepends = [
    base binary quickcheck-instances tasty tasty-hunit tasty-quickcheck
    text
  ];
  description = "Memory-efficient representation of Unicode text strings";
  license = lib.licenses.bsd3;
  
  src = fetchFromGitHub {
    owner = "haskell-hvr";
    repo = "text-short";
    rev = "d6286533f79b37ca510d8d9f1bd48e58e91d42b0";
    sha256 = "sha256-6izwX1RslgkKISzwuG9RMxxvgpryb0ria8PjPzaBrBg";
  };
}