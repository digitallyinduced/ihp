{ mkDerivation, base, lib, fetchFromGitHub }:
mkDerivation {
  pname = "base-prelude";
  version = "1.6.1";
  sha256 = "0rbx6k85svqrkw5ixp2xal8bg6xrz729g7rrhkgsr3ixv38k943j";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/nikita-volkov/base-prelude";
  description = "Featureful preludes formed solely from the \"base\" package";
  license = lib.licenses.mit;
  src = fetchFromGitHub {
    owner = "mpscholten";
    repo = "base-prelude";
    rev = "860ef6f407e797e8d2f9189ce7bd34d50cda208c";
    sha256 = "sha256-aFSn5d0eWWCycusL2ZYk8CMzivJplp/YP8HJx0X1ROk=";
  };
}