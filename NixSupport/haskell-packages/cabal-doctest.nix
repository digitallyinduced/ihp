{ mkDerivation, base, Cabal, directory, filepath, lib, fetchFromGitHub }:
mkDerivation {
  pname = "cabal-doctest";
  version = "1.0.8";
  sha256 = "2026a6a87d410202ce091412ca6bc33c5aca787025326b4a3d13425a23392e0e";
  revision = "2";
  editedCabalFile = "05v1awad3d1wvc763xcgvxm4n6n7bs7byc6s14kdbw35zcaddlcb";
  libraryHaskellDepends = [ base Cabal directory filepath ];
  homepage = "https://github.com/phadej/cabal-doctest";
  description = "A Setup.hs helper for doctests running";
  license = lib.licenses.bsd3;

  # https://github.com/haskellari/cabal-doctest/pull/71
  src = fetchFromGitHub {
    owner = "AlistairB";
    repo = "cabal-doctest";
    rev = "a76b6e1e37d17101731262efaae29a8b1e579f15";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i2";
  };
}