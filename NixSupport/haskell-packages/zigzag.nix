{ mkDerivation, base, lib, tasty, tasty-hunit, tasty-quickcheck }:
mkDerivation {
  pname = "zigzag";
  version = "0.0.1.0";
  sha256 = "ea55037681fa4f76061c52b0c97ad445869added1e8f9d5ecfdcf5f7c886c2bf";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  homepage = "https://github.com/byteverse/zigzag";
  description = "Zigzag encoding of integers into unsigned integers";
  license = lib.licenses.bsd3;
}
