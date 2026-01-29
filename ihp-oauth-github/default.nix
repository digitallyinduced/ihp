{ mkDerivation, aeson, base, ihp, lens, lib, text, transformers
, typerep-map, uri-encode, wai, wreq
}:
mkDerivation {
  pname = "ihp-oauth-github";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base ihp lens text transformers typerep-map uri-encode wai
    wreq
  ];
  description = "ihp-oauth-github";
  license = "unknown";
}
