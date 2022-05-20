{ mkDerivation
, base
, binary
, bytestring
, directory
, fetchgit
, filepath
, hs-brotli
, http-types
, lib
, mtl
, tasty
, tasty-hspec
, tasty-hunit
, unix
, wai
, wai-extra
}:
mkDerivation {
  pname = "wai-middleware-brotli";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/iand675/hs-brotli";
    sha256 = "1i4qaqn79m4jcg16pdm222xy6x97r5fv8ikyykf1nl79hs0dajrh";
    rev = "d7bce54b265883fb30a14d39d00cbf1c1308b2b1";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wai-middleware-brotli; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base binary bytestring directory filepath hs-brotli http-types unix wai ];
  testHaskellDepends = [ base bytestring hs-brotli http-types mtl tasty tasty-hspec tasty-hunit wai wai-extra ];
  homepage = "https://github.com/iand675/hs-brotli#readme";
  description = "WAI middleware for brotli compression";
  license = lib.licenses.bsd3;
}
