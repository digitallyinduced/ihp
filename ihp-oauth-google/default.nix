{ mkDerivation, aeson, base, ihp, jose-jwt, lens, lib, transformers
, typerep-map, uri-encode, wai, wreq
}:
mkDerivation {
  pname = "ihp-oauth-google";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base ihp jose-jwt lens transformers typerep-map uri-encode
    wai wreq
  ];
  description = "ihp-oauth-google";
  license = "unknown";
}
