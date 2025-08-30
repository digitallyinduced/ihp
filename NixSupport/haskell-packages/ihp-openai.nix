{ mkDerivation, lib, base
, text, http-streams, retry, io-streams, bytestring, aeson, HsOpenSSL
, hspec, neat-interpolation, safe-exceptions
}:
mkDerivation {
  pname = "ihp-openai";
  version = "v1.4.0";
  src = ./../../ihp-openai;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base text http-streams retry io-streams bytestring aeson HsOpenSSL safe-exceptions
  ];
  testHaskellDepends = [
    base text http-streams retry io-streams bytestring aeson HsOpenSSL hspec neat-interpolation
  ];
  enableLibraryForGhci = true;
  homepage = "https://ihp.digitallyinduced.com";
  configureFlags = ["--enable-optimization=2"];
  description = "Call GPT4 from IHP";
  license = lib.licenses.mit;
}