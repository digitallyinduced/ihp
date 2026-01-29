{ mkDerivation, ihp, lib, raven-haskell, wai, warp }:
mkDerivation {
  pname = "ihp-sentry";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ ihp raven-haskell wai warp ];
  description = "ihp-sentry";
  license = "unknown";
}
