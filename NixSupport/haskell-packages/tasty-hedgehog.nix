{ mkDerivation, base, hedgehog, lib, tagged, tasty
, tasty-expected-failure, fetchFromGitHub
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "1.3.0.0";
  sha256 = "1dk4bcm0a8vl1y5d3c89ypc688rp59fn11hyr2jx5kd8yvpzh0bj";
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  homepage = "https://github.com/qfpl/tasty-hedgehog";
  description = "Integration for tasty and hedgehog";
  license = lib.licenses.bsd3;
}