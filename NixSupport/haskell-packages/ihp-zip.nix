{ ihp, mkDerivation, base, stdenv, fetchFromGitHub, zip-archive
}:
mkDerivation {
  pname = "ihp-zip";
  version = "0.0.1";
  sha256 = "1ee92bece3c2bbf153dac013ee854fe8132702ee74cb61c07e7999ca1e35496d";
  libraryHaskellDepends = [
    base ihp zip-archive
  ];
  testHaskellDepends = [ ];
  homepage = "https://github.com/tippenein/countable-inflections";
  description = "Support for making ZIP Archives with IHP";
  license = stdenv.lib.licenses.mit;
  src = /Users/marc/digitallyinduced/ihp-zip;
}
