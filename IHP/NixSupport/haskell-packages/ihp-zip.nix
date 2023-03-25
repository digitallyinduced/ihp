{ ihp, mkDerivation, base, lib, fetchFromGitHub, zip-archive
}:
mkDerivation {
  pname = "ihp-zip";
  version = "0.0.1";
  sha256 = "1ee92bece3c2bbf153dac013ee854fe8132702ee74cb61c07e7999ca1e35496d";
  libraryHaskellDepends = [
    base ihp zip-archive
  ];
  testHaskellDepends = [ ];
  homepage = "https://github.com/digitallyinduced/ihp-zip";
  description = "Support for making ZIP Archives with IHP";
  license = lib.licenses.mit;
  src = fetchFromGitHub {
    owner = "digitallyinduced";
    repo = "ihp-zip";
    rev = "1c0d812d12d21269f83d6480a6ec7a8cdd054485";
    sha256 = "0y0dj8ggi1jqzy74i0d6k9my8kdvfi516zfgnsl7znicwq9laald";
  };
}
