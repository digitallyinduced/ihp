{ mkDerivation, base, comonad, hashable, keys, lib, pointed
, semigroupoids, semigroups, vector
, fetchFromGitHub
}:
mkDerivation {
  pname = "vector-instances";
  version = "3.4";
  sha256 = "1b0246ef0cf8372d61d5c7840d857f49299af2304b5107510377255ed4dd5381";
  libraryHaskellDepends = [
    base comonad hashable keys pointed semigroupoids semigroups vector
  ];
  homepage = "http://github.com/ekmett/vector-instances";
  description = "Orphan Instances for 'Data.Vector'";
  license = lib.licenses.bsd3;
  
  src = fetchFromGitHub {
    owner = "ekmett";
    repo = "vector-instances";
    rev = "31ca146c20f1ebbf312a88a6a3a21b5f3eba0072";
    sha256 = "sha256-6izwX1RslgkKISzwuG9RMxxvgpryb0ria8PjPzaBrBg";
  };
}