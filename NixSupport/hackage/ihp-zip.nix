{ mkDerivation, fetchgit, http-types, ihp, lib, wai, zip-archive }:
mkDerivation {
  pname = "ihp-zip";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/digitallyinduced/ihp-zip";
    sha256 = "0y0dj8ggi1jqzy74i0d6k9my8kdvfi516zfgnsl7znicwq9laald";
    rev = "1c0d812d12d21269f83d6480a6ec7a8cdd054485";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ http-types ihp wai zip-archive ];
  description = "ihp-zip";
  license = "unknown";
}
