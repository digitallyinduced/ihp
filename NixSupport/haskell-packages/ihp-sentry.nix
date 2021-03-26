{ ihp, mkDerivation, base, stdenv, fetchFromGitHub, raven-haskell, wai, warp
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
  license = stdenv.lib.licenses.mit;
  src = fetchFromGitHub {
    owner = "digitallyinduced";
    repo = "ihp-sentry";
    rev = "7439e6134dc59aaa843ce75bf98e60d601e5cb8b";
    sha256 = "1qpz1j1ygbssz5p0cr002f67vc2zc0crr6p2rj1k0755vf7cn9i1";
  };
}
