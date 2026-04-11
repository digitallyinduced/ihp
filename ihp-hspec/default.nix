{ mkDerivation, base, hasql, hspec, http-types, ihp, ihp-ide
, ihp-log, lib, process, text, uuid, vault, wai, wai-request-params
}:
mkDerivation {
  pname = "ihp-hspec";
  version = "1.5.0";
  src = ./.;
  libraryHaskellDepends = [
    base hasql hspec http-types ihp ihp-ide ihp-log process text uuid
    vault wai wai-request-params
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Test helpers for IHP apps";
  license = lib.licenses.mit;
}
