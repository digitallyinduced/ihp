{ mkDerivation, base, bytestring, cereal, hasql, hspec, http-types
, ihp, ihp-ide, ihp-log, lib, process, text, uuid, vault, wai
, wai-extra, wai-request-params, wai-session-maybe
}:
mkDerivation {
  pname = "ihp-hspec";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cereal hasql hspec http-types ihp ihp-ide ihp-log
    process text uuid vault wai wai-extra wai-request-params
    wai-session-maybe
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Test helpers for IHP apps";
  license = lib.licenses.mit;
}
