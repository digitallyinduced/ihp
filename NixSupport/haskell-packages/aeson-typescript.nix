{ mkDerivation, aeson, base, bytestring, containers, directory
, fetchgit, filepath, hpack, hspec, lib, mtl, process
, string-interpolate, template-haskell, temporary, text
, th-abstraction, transformers, unordered-containers
}:
mkDerivation {
  pname = "aeson-typescript";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/codedownio/aeson-typescript";
    sha256 = "1bm2s3xc5szvj5pcgh60zj00cyl84sh38743qyn31i9m61r9grnb";
    rev = "305cd581fce927ec3a5792ee2cf8b0e7208d901e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson base containers mtl string-interpolate template-haskell text
    th-abstraction transformers unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec mtl
    process string-interpolate template-haskell temporary text
    th-abstraction transformers unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/codedownio/aeson-typescript#readme";
  description = "Generate TypeScript definition files from your ADTs";
  license = lib.licenses.bsd3;
}
