{ mkDerivation, array, base, bytestring, containers, lib
, semigroups, unordered-containers
, fetchFromGitHub
}:
mkDerivation {
  pname = "charset";
  version = "0.3.7.1";
  sha256 = "3d415d2883bd7bf0cc9f038e8323f19c71e07dd12a3c712f449ccb8b4daac0be";
  revision = "1";
  editedCabalFile = "1z6nxw2g9vgsjq0g159sk8mwj68lwzxzi5iv5ynha0h85jcqxszy";
  libraryHaskellDepends = [
    array base bytestring containers semigroups unordered-containers
  ];
  homepage = "http://github.com/ekmett/charset";
  description = "Fast unicode character sets based on complemented PATRICIA tries";
  license = lib.licenses.bsd3;
  src = fetchFromGitHub {
    owner = "ekmett";
    repo = "charset";
    rev = "e824d8f1c43e7c4f4b46448d6f317d7dbb2e0928";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i7";
  };
}