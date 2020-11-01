{ mkDerivation, base, base16-bytestring, bytestring, cryptonite
, memory, stdenv, stripe-concepts, text
}:
mkDerivation {
  pname = "stripe-signature";
  version = "1.0.0.6";
  sha256 = "0lp3fli9g5yvlxy8f0md2d3wv6z45mw0929b8c0y2xkcsdjvpp5l";
  libraryHaskellDepends = [
    base base16-bytestring bytestring cryptonite memory stripe-concepts
    text
  ];
  testHaskellDepends = [ base bytestring text ];
  homepage = "https://github.com/typeclasses/stripe";
  description = "Verification of Stripe webhook signatures";
  license = stdenv.lib.licenses.mit;
}