{ ihp, mkDerivation, base, lib, fetchFromGitHub, raven-haskell, wai, warp
}:
mkDerivation {
  pname = "ihp-sentry";
  version = "0.0.1";
  sha256 = "1ee92bece3c2bbf153dac013ee854fe8132702ee74cb61c07e7999ca1e35496d";
  libraryHaskellDepends = [
    base ihp raven-haskell wai warp
  ];
  testHaskellDepends = [ ];
  homepage = "https://github.com/digitallyinduced/ihp-sentry";
  description = "Track exceptions in your IHP apps with sentry";
  license = {
    fullName = "IHP Pro License";
    url = "https://ihp.digitallyinduced.com/license/";
    free = true; # Not actually free, but we want to make the install seamless
  };
  src = ./../../Plugins/ihp-sentry;
}
