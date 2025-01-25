{ mkDerivation, base, http-types, lib, network, systemd, unix, wai
, warp, fetchFromGitHub
}:
mkDerivation {
  pname = "warp-systemd";
  version = "0.3.0.0";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base network systemd unix wai warp ];
  executableHaskellDepends = [ base http-types wai warp ];
  homepage = "https://github.com/hercules-ci/warp-systemd";
  description = "Socket activation and other systemd integration for the Warp web server (WAI)";
  license = lib.licenses.bsd3;
  mainProgram = "warp-systemd-example";
  src = fetchFromGitHub {
    owner = "hercules-ci";
    repo = "warp-systemd";
    rev = "6824c99cbe8e433e42684e78aed36b895beed323";
    sha256 = "sha256-euSzPbH7J1R1g5qnXpeuGcrJNA4717FzYhHmKYledfg=";
  };
}