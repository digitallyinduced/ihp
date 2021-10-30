{ mkDerivation, base, comonad, containers, data-default-class
, hashable, kan-extensions, lib, semigroupoids, semigroups, stm
, tagged, transformers, transformers-compat, unordered-containers
, fetchFromGitHub
, appar, byteorder, network
}:
mkDerivation {
  pname = "pointed";
  version = "5.0.2";
  sha256 = "b8ba3d7c1e4a4fcb3f3c7f1c0a9f4d237bdf45e93ba7a2fad07ec5268c17e91e";
  libraryHaskellDepends = [
    base comonad containers data-default-class hashable kan-extensions
    semigroupoids semigroups stm tagged transformers
    transformers-compat unordered-containers
    appar byteorder network
  ];
  homepage = "http://github.com/ekmett/pointed/";
  description = "Pointed and copointed data";
  license = lib.licenses.bsd3;
  # https://github.com/ekmett/pointed/commit/01b619dbc2a0be9a0efd1f04a499169d5069feef
  src = fetchFromGitHub {
    owner = "ekmett";
    repo = "pointed";
    rev = "01b619dbc2a0be9a0efd1f04a499169d5069feef";
    sha256 = "sha256-HmnlUekRHJmPAFb+LuiC2KL6F4Mt0obpjoOx2F8OHGs";
  };
}