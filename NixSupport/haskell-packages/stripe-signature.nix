{ mkDerivation, base, base16-bytestring, bytestring, cryptonite
, memory, stdenv, stripe-concepts, text
}:
mkDerivation {
  pname = "stripe-signature";
  version = "1.0.0.10";
  sha256 = "04b5z6hnm000fii4qiczm4xpr41v55fgcj07yh35iwh4gwgc7c4h";
  libraryHaskellDepends = [
    base base16-bytestring bytestring cryptonite memory stripe-concepts
    text
  ];
  testHaskellDepends = [ base bytestring text ];
  homepage = "https://github.com/typeclasses/stripe";
  description = "Verification of Stripe webhook signatures";
  license = stdenv.lib.licenses.mit;
}