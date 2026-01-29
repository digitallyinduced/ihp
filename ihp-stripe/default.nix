{ mkDerivation, aeson, base, containers, ihp, ihp-log, lens, lib
, strip-ansi-escape, stripe-concepts, stripe-signature, time
, transformers, typerep-map, uri-encode, wai, wreq
}:
mkDerivation {
  pname = "ihp-stripe";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers ihp ihp-log lens strip-ansi-escape
    stripe-concepts stripe-signature time transformers typerep-map
    uri-encode wai wreq
  ];
  description = "ihp-stripe";
  license = "unknown";
}
