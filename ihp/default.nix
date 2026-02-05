{ mkDerivation, aeson, async, attoparsec, base, basic-prelude
, binary, blaze-html, blaze-markup, bytestring, case-insensitive
, cereal, cereal-text, classy-prelude, clientsession, conduit-extra
, containers, cookie, countable-inflections, data-default, deepseq
, directory, fast-logger, filepath, ghc-prim, hashable
, hasql, hasql-pool, hasql-dynamic-statements
, haskell-src-exts, haskell-src-meta, hspec, http-client
, http-client-tls, http-media, http-types, ihp-context, ihp-hsx
, ihp-imagemagick, ihp-log, ihp-modal, ihp-pagehead
, ihp-pglistener
, ihp-postgresql-simple-extra, inflections, interpolate, ip, lens
, lib, mime-types, minio-hs, mono-traversable, mtl
, neat-interpolation, network, network-uri, parser-combinators
, postgresql-simple, process, pwstore-fast, random, random-strings
, regex-tdfa, resource-pool, resourcet, safe-exceptions, scientific
, slugger, split, stm, string-conversions, template-haskell
, temporary, text, time, transformers, typerep-map, unagi-chan
, unix, unliftio, unordered-containers, uri-encode, uuid, vault
, vector, wai, wai-app-static, wai-asset-path, wai-cors, wai-extra
, wai-flash-messages, wai-request-params, wai-session
, wai-session-clientsession, wai-util, wai-websockets, warp
, warp-systemd, websockets, with-utf8, wreq
}:
mkDerivation {
  pname = "ihp";
  version = "1.4.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson async attoparsec base basic-prelude binary blaze-html
    blaze-markup bytestring case-insensitive cereal cereal-text
    classy-prelude clientsession conduit-extra containers cookie
    countable-inflections data-default deepseq directory fast-logger
    filepath ghc-prim hashable hasql hasql-pool hasql-dynamic-statements
    haskell-src-exts haskell-src-meta hspec
    http-client http-client-tls http-media http-types ihp-context
    ihp-hsx ihp-imagemagick ihp-log ihp-modal ihp-pagehead
    ihp-pglistener
    ihp-postgresql-simple-extra inflections interpolate ip lens
    mime-types minio-hs mono-traversable mtl neat-interpolation network
    network-uri parser-combinators postgresql-simple process
    pwstore-fast random random-strings regex-tdfa resource-pool
    resourcet safe-exceptions scientific slugger split stm
    string-conversions template-haskell temporary text time
    transformers typerep-map unagi-chan unix unliftio
    unordered-containers uri-encode uuid vault vector wai
    wai-app-static wai-asset-path wai-cors wai-extra wai-flash-messages
    wai-request-params wai-session wai-session-clientsession wai-util
    wai-websockets warp warp-systemd websockets with-utf8 wreq
  ];
  testHaskellDepends = [
    aeson async attoparsec base basic-prelude binary blaze-html
    blaze-markup bytestring case-insensitive cereal cereal-text
    classy-prelude clientsession conduit-extra containers cookie
    countable-inflections data-default deepseq directory fast-logger
    filepath ghc-prim hashable hasql hasql-pool hasql-dynamic-statements
    haskell-src-exts haskell-src-meta hspec
    http-client http-client-tls http-media http-types ihp-context
    ihp-hsx ihp-imagemagick ihp-log ihp-modal ihp-pagehead
    ihp-pglistener
    ihp-postgresql-simple-extra inflections interpolate ip lens
    mime-types minio-hs mono-traversable mtl neat-interpolation network
    network-uri parser-combinators postgresql-simple process
    pwstore-fast random random-strings regex-tdfa resource-pool
    resourcet safe-exceptions scientific slugger split stm
    string-conversions template-haskell temporary text time
    transformers typerep-map unagi-chan unix unliftio
    unordered-containers uri-encode uuid vault vector wai
    wai-app-static wai-asset-path wai-cors wai-extra wai-flash-messages
    wai-request-params wai-session wai-session-clientsession wai-util
    wai-websockets warp warp-systemd websockets with-utf8 wreq
  ];
  homepage = "https://ihp.digitallyinduced.com/";
  description = "Haskell Web Framework";
  license = lib.licenses.mit;
}
