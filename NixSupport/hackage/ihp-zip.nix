{ mkDerivation, fetchgit, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/digitallyinduced/ihp-zip";
    sha256 = "116ly1ll3d9m702f7cwm5dbxpnyg4afjhg9yg3f0qi775db9vcgv";
    rev = "62f632d23ee34b9783b82b53ab72e6ba5d716b76";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ http-types ihp wai zip-archive ];
  description = "ihp-zip";
  license = "unknown";
}
