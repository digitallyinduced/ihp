{ ihp, mkDerivation, base, lib, fetchFromGitHub
}:
mkDerivation {
  pname = "ihp-oauth-github";
  version = "0.0.1";
  sha256 = "1ee92bece3c2bbf153dac013ee854fe8132702ee74cb61c07e7999ca1e35496d";
  libraryHaskellDepends = [
    base ihp
  ];
  testHaskellDepends = [ ];
  homepage = "https://github.com/digitallyinduced/ihp-pro";
  description = "Login with GitHub";
  license = {
    fullName = "IHP Pro License";
    url = "https://ihp.digitallyinduced.com/license/";
    free = true; # Not actually free, but we want to make the install seamless
  };
  src = ./../../Plugins/ihp-oauth-github;
}