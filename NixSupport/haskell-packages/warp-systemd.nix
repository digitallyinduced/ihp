{ mkDerivation, base, http-types, lib, network, systemd, unix, wai
, warp
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
}