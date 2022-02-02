{ mkDerivation, base, base-compat, bytestring, case-insensitive
, fetchzip, hpack, hspec, hspec-core, hspec-expectations
, http-types, lib, QuickCheck, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "hspec-wai";
  version = "0.11.1";
  src = fetchzip {
    url = "https://github.com/hspec/hspec-wai/archive/refs/heads/master.zip";
    sha256 = "0p3vj2f1lp5vyil4872pw4fkx9qsggwg3acla2mcqvpah4f7iv50";
  };
  libraryHaskellDepends = [
    base base-compat bytestring case-insensitive hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base base-compat bytestring case-insensitive hspec hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hspec/hspec-wai#readme";
  description = "Experimental Hspec support for testing WAI applications";
  license = lib.licenses.mit;
}
