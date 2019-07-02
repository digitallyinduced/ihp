{ mkDerivation, base, bytestring, exceptions, hspec, pcre-utils
, QuickCheck, regex-pcre-builtin, stdenv, text, fetchFromGitHub
}:
mkDerivation {
  pname = "countable-inflections";
  version = "0.2.0";
  sha256 = "1ee92bece3c2bbf153dac013ee854fe8132702ee74cb61c07e7999ca1e35496d";
  libraryHaskellDepends = [
    base bytestring exceptions pcre-utils regex-pcre-builtin text
  ];
  testHaskellDepends = [ base hspec QuickCheck text ];
  homepage = "https://github.com/tippenein/countable-inflections";
  description = "Countable Text Inflections";
  license = stdenv.lib.licenses.mit;
  src = fetchFromGitHub {
    owner = "tippenein";
    repo = "countable-inflections";
    rev = "7526fc8c17edc2d951a86c13659dd8206f5c2efa";
    sha256 = "0d0s81mnv8rava611h4n065ghkr4r50xjlxh5gq683cv51d33kgr";
  };
}
