{ mkDerivation, async, base, basic-prelude, binary, blaze-html
, bytestring, containers, hasql, hasql-dynamic-statements
, hasql-pool, ihp, ihp-context, ihp-hsx, ihp-log, ihp-pglistener
, lib, string-conversions, text, time, typerep-map, uuid, vault
, wai, wai-request-params, websockets
}:
mkDerivation {
  pname = "ihp-auto-refresh";
  version = "1.4.0";
  src = ./.;
  libraryHaskellDepends = [
    async base basic-prelude binary blaze-html bytestring containers
    hasql hasql-dynamic-statements hasql-pool ihp ihp-context ihp-hsx
    ihp-log ihp-pglistener string-conversions text time typerep-map
    uuid vault wai wai-request-params websockets
  ];
  description = "AutoRefresh for IHP";
  license = lib.licenses.mit;
}
