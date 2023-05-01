{ ihp, mkDerivation, base, lib, fetchFromGitHub, raven-haskell, wai, warp
}:
mkDerivation {
  pname = "ihp-sentry";
  version = "0.0.1";
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
