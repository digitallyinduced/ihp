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
, unix
, wai
}:
mkDerivation {
  pname = "wai-middleware-brotli";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/s0kil/hs-brotli";
    sha256 = "0xwp0zj28njl6f8x47rwbsngkbg6r9a05bhrdxhkls2gnbvx8dac";
    rev = "9f27b1f443d21d177100cd7d0906ad68020d9331";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/wai-middleware-brotli; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base
    binary
    bytestring
    directory
    filepath
    hs-brotli
    http-types
    unix
    wai
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/s0kil/hs-brotli#readme";
  description = "WAI middleware for brotli compression";
  license = lib.licenses.bsd3;
}
