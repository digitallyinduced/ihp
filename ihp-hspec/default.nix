{ mkDerivation, base, hasql, ihp, ihp-ide, ihp-log, lib, process
, text, uuid, vault, wai, wai-request-params
}:
mkDerivation {
  pname = "ihp-hspec";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    base hasql ihp ihp-ide ihp-log process text uuid vault wai
    wai-request-params
  ];
  description = "Test helpers for IHP apps";
  license = lib.licenses.mit;
}
