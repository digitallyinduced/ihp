{ mkDerivation, aeson, attoparsec-aeson, base, bytestring
, HsOpenSSL, hspec, http-streams, io-streams, lib
, neat-interpolation, network-uri, retry, safe-exceptions, text
}:
mkDerivation {
  pname = "ihp-openai";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec-aeson base bytestring HsOpenSSL http-streams
    io-streams network-uri retry safe-exceptions text
  ];
  testHaskellDepends = [ aeson base hspec neat-interpolation text ];
  description = "Call GPT4 from your Haskell apps";
  license = lib.licenses.mit;
}
